package scala.tools.refactoring
package implementations

import common.Change
import common.PimpedTrees
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.util.TransparentPosition

abstract class MethodSignatureRefactoring extends MultiStageRefactoring with common.InteractiveScalaCompiler with PimpedTrees with analysis.Indexes with common.TreeTraverser{

  import global._
  
  case class AffectedDef(defSymbol: Symbol, nrParamLists: Int)
  case class AffectedDefs(originals: List[AffectedDef], partials: List[AffectedDef])
  
  type PreparationResult = (DefDef, AffectedDefs)
  
  def prepare(s: Selection) = {
    
    def accessor(v: ValDef) = {
      val root = cuRoot(v.pos)
      val p: Tree => Boolean = (tree: Tree) => tree match {
        case defdef: DefDef => v.nameString equals defdef.nameString
        case _ => false
      }
      val traverser = new global.FilterTreeTraverser(p)
      traverser.traverse(root.get)
      traverser.hits.toList match {
        case Nil => None
        case x::Nil => Some(x.symbol)
        case _ => None
      }
    }
    
    trait GenericPartialsFinder[RHS] {
      trait Extractable[In, Out] {
        def unapply(i: In): Option[Out]
      }
      
      val ContainedSymbol: Extractable[RHS, Symbol]
      val Nested: Extractable[RHS, RHS]
    
      def containsCall(r: RHS, symbol: Symbol): Boolean = r match {
        case ContainedSymbol(s) => symbol == s
        case Nested(n) => containsCall(n, symbol)
        case _ => false
      }
      
      def nrParamLists(r: RHS, origNrParamLists: Int): Int
      
      def isPartial(defSymbol: Symbol)(tree: Tree): Boolean
      
      val PartialDefDef: Extractable[Tree, (DefDef, RHS)]
      val PartialValDef: Extractable[Tree, (ValDef, RHS)]
      
      def matchForPartial[T](
          candidate: Tree, 
          defSymbol: Symbol, 
          handleDefDefMatch: (DefDef, RHS) => T, 
          handleValDefMatch: (ValDef, RHS) => T,
          handleNonMatch: => T): T = candidate match {
        case PartialDefDef(defdef, rhs) if containsCall(rhs, defSymbol) => handleDefDefMatch(defdef, rhs)
        case PartialValDef(valdef, rhs) if containsCall(rhs, defSymbol) => handleValDefMatch(valdef, rhs)
        case _ => handleNonMatch
      }
             
      def findPartialsForDef(defAndNrParamLists: (Symbol, Int)) = {
        val defSymbol = defAndNrParamLists._1
        val origNrParamLists = defAndNrParamLists._2
        val allOccurences = index.occurences(defSymbol)
        val cuRoots = (allOccurences flatMap (occ => cuRoot(occ.pos))).distinct
        val hits = cuRoots flatMap {
          cuRoot =>
            val traverser = new global.FilterTreeTraverser(isPartial(defSymbol))
            traverser.traverse(cuRoot)
            traverser.hits.toList
        }
        
        val handleDefDef = (d: DefDef, f: RHS) => {
          Some(d.symbol, nrParamLists(f, origNrParamLists))
        }
        
        val handleValDef = (v: ValDef, f: RHS) => {
          accessor(v) map ((_, nrParamLists(f, origNrParamLists)))
        }
        
        hits flatMap {
          matchForPartial(_, defSymbol, handleDefDef, handleValDef, None)
        }
      }
      
      val needsRecursion = false
      
      def findPartials(defs: List[(Symbol, Int)], acc: List[(Symbol, Int)] = Nil): List[(Symbol, Int)] = {
        val partials = defs flatMap findPartialsForDef
        if(needsRecursion) {
          partials match {
            case Nil => acc
            case ps => findPartials(partials, acc:::partials)
          }
        } else {
          partials
        }
      }
      
    }
    
    object PartialsFinder extends GenericPartialsFinder[Function] {

      override val ContainedSymbol = new Extractable[Function, Symbol] {
        override def unapply(f: Function) = f match {
          case Function(_, a: Apply) => Some(a.symbol)
          case _ => None
        }
      }
      
      override val Nested = new Extractable[Function, Function]{
        def unapply(f: Function): Option[Function] = f match {
          case Function(_, g: Function) => Some(g)
          case _ => None
        }
      }
      
      override def nrParamLists(f: Function, origNrParamLists: Int): Int = f match {
        case Function(_, g: Function) => 1 + nrParamLists(g, origNrParamLists)
        case _ => 1
      }
      
      override def isPartial(defSymbol: Symbol)(tree: Tree) = 
        matchForPartial(tree, defSymbol, (d, f) => true, (v, f) => true, false)
      
      override val PartialDefDef =  new Extractable[Tree, (DefDef, Function)] {
        def unapply(tree: Tree) = tree match {
          case defdef @ DefDef(_, _, _, _, _, Block(Nil, fun: Function)) => Some(defdef, fun)
          case _ => None
        }
      }
      
      override val PartialValDef = new Extractable[Tree, (ValDef, Function)] {
        def unapply(tree: Tree) = tree match {
          case valdef @ ValDef(_, _, _, Block(Nil, fun: Function)) => Some(valdef, fun)
          case _ => None
        }
      }
    }
    
    object PartialPartialsFinder extends GenericPartialsFinder[Apply] {
      
      override val ContainedSymbol = new Extractable[Apply, Symbol] {
        def unapply(a: Apply) = a match {
          case Apply(Select(s: Select, _), _) => Some(s.symbol)
          case _ => None
        }
      }
      
      override val Nested = new Extractable[Apply, Apply] {
        def unapply(apply: Apply) = apply match {
          case Apply(Select(a: Apply, _), _) => Some(a)
          case _ => None
        }
      }
      
      override def nrParamLists(apply: Apply, origNrParamLists: Int): Int = 
        origNrParamLists - nrAppliesInPartial(apply)
      
      def nrAppliesInPartial(apply: Apply): Int = apply match {
        case Apply(Select(qualifier: Apply, _), _) => 1 + nrAppliesInPartial(qualifier)
        case Apply(select: Select, _) => 1
        case _ => 0
      }
      
      override def isPartial(defSymbol: Symbol)(tree: Tree) = {
        matchForPartial(tree, defSymbol, (d, a) => true, (v, a) => true, false)
      }
      
      override val PartialDefDef = new Extractable[Tree, (DefDef, Apply)] {
        def unapply(tree: Tree) = tree match {
          case defdef @ DefDef(_, _, _, _, _, a: Apply) => Some(defdef, a)
          case _ => None
        }
      }
      
      override val PartialValDef = new Extractable[Tree, (ValDef, Apply)] {
        def unapply(tree: Tree) = tree match {
          case valdef @ ValDef(_, _, _, a: Apply) => Some(valdef, a)
          case _ => None
        }
      }
      
      override val needsRecursion = true
    }
    
    s.findSelectedOfType[DefDef] match {
      case None => Left(PreparationError("no defdef selected"))
      case Some(selectedDefDef) => {
        val originalSymbols = index.overridesInClasses(selectedDefDef.symbol)
        val originals = originalSymbols map (AffectedDef(_, selectedDefDef.vparamss.size))
        val partialDefs = PartialsFinder.findPartials(originalSymbols map ((_, selectedDefDef.vparamss.size)))
        val partials = partialDefs flatMap (t => index.overridesInClasses(t._1) map (AffectedDef(_, t._2)))
        val partialPartialDefs = PartialPartialsFinder.findPartials(partialDefs)
        val partialPartials = partialPartialDefs flatMap (t => index.overridesInClasses(t._1) map (AffectedDef(_, t._2)))
        
        Right((selectedDefDef, AffectedDefs(originals, partials:::partialPartials)))
      }
    }
  }
  
  override def perform(selection: Selection, prep: PreparationResult, originalParams: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    if(!checkRefactoringParams(prep, originalParams))
      return Left(RefactoringError("invalid refactoring params for method signature refactoring"))

    def findDef(defdef: DefTree)= filter {
      case d: DefDef => d == defdef
    }
      
    def refactorDefDef(defdef: DefTree, params: RefactoringParameters) = topdown {
      matchingChildren {
        findDef(defdef) &> defdefRefactoring(params)
      }
    }
    
    def filterApply(applySymbol: Symbol) = filter {
      case apply: Apply if apply.pos.isOpaqueRange => apply.symbol.fullName == applySymbol.fullName
    }
    
    def filterPartialApply(applySymbol: Symbol) =  {
      def isPartialApply(apply: Apply): Boolean = apply match {
        case Apply(Select(qualifier: Select, _), _) if qualifier.hasSymbol => qualifier.symbol == applySymbol
        case Apply(Select(qualifier: Apply, _), _) => isPartialApply(qualifier)
        case Apply(childApply: Apply, _) => isPartialApply(childApply)
        case _ => false
      }
      
      filter {
      case apply: Apply => isPartialApply(apply)
    }
  }
    
    def refactorCalls(callFilter: Symbol => Transformation[Tree, Tree])(applySymbol: Symbol, params: RefactoringParameters) = traverseApply {
      matchingChildren {
        callFilter(applySymbol) &> applyRefactoring(params)
      }
    }
    
    def refactorOrdinaryCalls = refactorCalls(filterApply) _
    def refactorPartialCalls = refactorCalls(filterPartialApply) _
    
    val refactorMethodSignature = {
      val originalNrParamLists = originalParams
      val affectedDefDefs = prep._2
      val allDefDefSymbols = affectedDefDefs.originals
      val allPartialSymbols = affectedDefDefs.partials
      val singleRefactorings = allDefDefSymbols map (d => 
        refactorDefDef(index.declaration(d.defSymbol).get, originalParams) &> 
        refactorOrdinaryCalls(d.defSymbol, prepareParamsForSingleRefactoring(originalParams, prep._1, d)))
      val singlePartialRefactorings = allPartialSymbols map 
        (p => refactorPartialCalls(p.defSymbol, prepareParamsForSingleRefactoring(originalParams, prep._1, p)))
      val refactoring = (singleRefactorings:::singlePartialRefactorings).foldLeft(id[Tree])((t, c) => t &> c)
      refactoring
    }
    
    val affectedCus = {
      val affectedDefs = prep._2
      val originalsSymbols = affectedDefs.originals.map(_.defSymbol)
      val partialsSymbols = affectedDefs.partials.map(_.defSymbol)
      val allSymbols: List[Symbol] = prep._1.symbol::originalsSymbols:::partialsSymbols
      val occurences = allSymbols.map(index.occurences)
      occurences.flatten.flatMap(t => cuRoot(t.pos)).distinct
    }
    
    val changedTrees = affectedCus flatMap (refactorMethodSignature(_))
    
    Right(refactor(changedTrees))
  }
  
  def checkRefactoringParams(selectedValue: PreparationResult, params: RefactoringParameters): Boolean
  
  def defdefRefactoring(params: RefactoringParameters): Transformation[Tree, Tree]
  
  def applyRefactoring(params: RefactoringParameters): Transformation[Tree, Tree]
  
  def paramListPos(fun: Option[Tree]): Int = fun match {
    case Some(Apply(Select(qualifier, _), _)) => 1 + paramListPos(Some(qualifier))
    case Some(Apply(apply: Apply, _)) => 1 + paramListPos(Some(apply))
    case _ => 0
  }
    
  def traverseApply[X <% (X ⇒ X) ⇒ X](t: => Transformation[X, X]) = topdown(t)
  
  def prepareParamsForSingleRefactoring(originalParams: RefactoringParameters, selectedMethod: DefDef, toRefactor: AffectedDef): RefactoringParameters = originalParams
  
}