package scala.tools.refactoring
package implementations

import common.Change
import common.PimpedTrees
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.util.TransparentPosition

abstract class MethodSignatureRefactoring extends MultiStageRefactoring with common.InteractiveScalaCompiler with PimpedTrees with analysis.Indexes with common.TreeTraverser{

  import global._
  
  case class AffectedDefDef(defdef: Symbol, nrParamLists: Int)
  case class AffectedDefDefs(originals: List[AffectedDefDef], partials: List[AffectedDefDef])
  
  type PreparationResult = (DefDef, AffectedDefDefs)
  
  def prepare(s: Selection) = {
    def findPartials(defdefs: List[Symbol]) = {
      
      def containsCall(f: Function, symbol: Symbol): Boolean = f match {
        case Function(_, Apply(fun, args)) if symbol == fun.symbol => true
        case Function(_, g: Function) => containsCall(g, symbol)
        case _ => false
      }
      
      def nrParamLists(f: Function): Int = f match {
        case Function(_, g: Function) => 1 + nrParamLists(g)
        case _ => 1
      }
      
      def matchForPartial[T](
          candidate: Tree, 
          defdefSymbol: Symbol, 
          handleMatch: (DefDef, Function) => T, 
          handleNonMatch: => T): T = candidate match {
        case defdef @ DefDef(_, _, _, _, _, Block(Nil, fun: Function)) if containsCall(fun, defdefSymbol) => handleMatch(defdef, fun)
        case _ => handleNonMatch
      }
      
      def isPartial(defdefSymbol: Symbol)(tree: Tree) = 
        matchForPartial(tree, defdefSymbol, (d: DefDef, f: Function) => true, false)
      
      def findPartialsForDefDef(defdef: Symbol) = {
        val allOccurences = index.occurences(defdef)
        val cuRoots = (allOccurences flatMap (occ => cuRoot(occ.pos))).distinct
        val hits = cuRoots flatMap {
          cuRoot =>
            val traverser = new global.FilterTreeTraverser(isPartial(defdef))
            traverser.traverse(cuRoot)
            traverser.hits.toList
        } 
        hits flatMap {
          matchForPartial(_, defdef, (d: DefDef, f: Function) => Some(d, nrParamLists(f)), None)
        }
      }
      
      defdefs flatMap findPartialsForDefDef
    }
    
    def findPartialPartials(
        partials: List[(DefDef, Int)], 
        acc: List[(DefDef, Int)] = Nil): List[(DefDef, Int)] = {
      def containsPartialCall(apply: Apply, partialSymbol: Symbol): Boolean = apply match {
        case Apply(Select(s: Select, _), _) => partialSymbol == s.symbol
        case Apply(Select(a: Apply, _), _) => containsPartialCall(a, partialSymbol)
        case _ => false
      }
      
      def nrAppliesInPartial(apply: Apply): Int = apply match {
        case Apply(Select(qualifier: Apply, _), _) => 1 + nrAppliesInPartial(qualifier)
        case Apply(select: Select, _) => 1
        case _ => 0
      }
      
      def matchForPartialPartial[T](
          candidate: Tree,
          partialDefDefSymbol: Symbol,
          handleMatch: (DefDef, Apply) => T,
          handleNonMatch: => T): T = candidate match {
        case defdef @ DefDef(_, _, _, _, _, a: Apply) if containsPartialCall(a, partialDefDefSymbol) => handleMatch(defdef, a)
        case _ => handleNonMatch
      }
      
      def isPartialPartial(partialDefDefSymbol: Symbol)(tree: Tree) = {
        matchForPartialPartial(tree, partialDefDefSymbol, (d: DefDef, a: Apply) => true, false)
      }
      
      def findPartialPartialsForDefDef(partialDefDef: (Symbol, Int)) = {
        def findPartials(defdef: Symbol) = {
          val allOccurences = index.occurences(defdef)
          val cuRoots = (allOccurences flatMap (occ => cuRoot(occ.pos))).distinct
          cuRoots flatMap {
            cuRoot =>
              val traverser = new global.FilterTreeTraverser(isPartialPartial(defdef))
              traverser.traverse(cuRoot)
              traverser.hits.toList
          }
        }
        
        val hits = findPartials(partialDefDef._1)
        hits flatMap {
          matchForPartialPartial(_, partialDefDef._1, (d: DefDef, a: Apply) => Some(d, partialDefDef._2 - nrAppliesInPartial(a)), None)
        }
      }
      val partialSymbols = partials map (t => (t._1.symbol, t._2))
      val partialPartials = partialSymbols flatMap (findPartialPartialsForDefDef)
      partialPartials match {
        case Nil => acc
        case ps => findPartialPartials(partialPartials, acc:::partialPartials)
      }
    }
    
    s.findSelectedOfType[DefDef] match {
      case None => Left(PreparationError("no defdef selected"))
      case Some(d) => {
        val selectedDefDef = d
        val originalSymbols = index.overridesInClasses(selectedDefDef.symbol)
        val originals = originalSymbols map (defdef => AffectedDefDef(defdef, selectedDefDef.vparamss.size))
        val partialDefDefs = findPartials(originalSymbols)
        val partials = partialDefDefs flatMap (t => index.overridesInClasses(t._1.symbol) map (AffectedDefDef(_, t._2)))
        val partialPartialDefDefs = findPartialPartials(partialDefDefs)
        val partialPartials = partialPartialDefDefs flatMap (t => index.overridesInClasses(t._1.symbol) map (AffectedDefDef(_, t._2)))
        
        Right((selectedDefDef, AffectedDefDefs(originals, partials:::partialPartials)))
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
        refactorDefDef(index.declaration(d.defdef).get, originalParams) &> 
        refactorOrdinaryCalls(d.defdef, prepareParamsForSingleRefactoring(originalParams, d.nrParamLists)))
      val singlePartialRefactorings = allPartialSymbols map 
        (p => refactorPartialCalls(p.defdef, prepareParamsForSingleRefactoring(originalParams, p.nrParamLists)))
      val refactoring = (singleRefactorings:::singlePartialRefactorings).foldLeft(id[Tree])((t, c) => t &> c)
      refactoring
    }
    
    Right(transformFile(selection.file, refactorMethodSignature))
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
  
  def prepareParamsForSingleRefactoring(originalParams: RefactoringParameters, nrParamLists: Int): RefactoringParameters = originalParams
  
}