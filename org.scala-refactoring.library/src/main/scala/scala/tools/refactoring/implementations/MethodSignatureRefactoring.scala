package scala.tools.refactoring
package implementations

import scala.Option.option2Iterable
import scala.tools.refactoring.MultiStageRefactoring

import analysis.PartiallyAppliedMethodsFinder
import common.Change
import common.PimpedTrees

/**
 * Generic implementation of a refactoring that changes a method signature
 * (i.e. does something with the parameters).
 * This generic implementation tries to detect all occurrences of the selected
 * method, this includes definitions of other methods that are partial applications
 * of the original one (and recursively) and calls to all those methods.
 * An concrete implementation of a method signature refactoring only has to provide the
 * actual transformations of a definition and an application of a method.
 */
abstract class MethodSignatureRefactoring extends MultiStageRefactoring with common.InteractiveScalaCompiler with PimpedTrees with analysis.Indexes with common.TreeTraverser with PartiallyAppliedMethodsFinder {

  import global._

  case class AffectedDefs(originals: List[DefInfo], partials: List[DefInfo])

  /**
   * defdef is the selected method that should be refactored.
   */
  case class MethodSignaturePrepResult(defdef: DefDef)
  type PreparationResult = MethodSignaturePrepResult

  def prepare(s: Selection) = {
    s.findSelectedOfType[DefDef].map {
      selectedDefDef =>
        MethodSignaturePrepResult(selectedDefDef)
    }.toRight(PreparationError("No method defintion selected."))
  }

  override def perform(selection: Selection, prep: PreparationResult, originalParams: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    val selectedDefDefSymbol = {
      val symbols = index.positionToSymbol(prep.defdef.pos)
      symbols.filter(_.isMethod).headOption getOrElse {return Left(RefactoringError("failed to find the symbol of the selected method"))}
    }

   /*
   * affectedDefs contains all DefDefs and ValDefs that have to join the refactoring. These includes
   * all partial applications of the selected defdef and all its versions in the inheritance hierarchy
   * (and all this recursively)
   */
    val affectedDefs = {
      val originalSymbols = index.overridesInClasses(selectedDefDefSymbol)
      val originals = originalSymbols map (s => DefInfo(s, prep.defdef.vparamss.size))
      val partialDefs = PartialsFinder.findPartials(originalSymbols map (DefInfo(_, prep.defdef.vparamss.size)))
      val partials = partialDefs flatMap (d => index.overridesInClasses(d.symbol) map (s => DefInfo(s, d.nrParamLists, d.nrUntouchableParamLists)))
      val partialPartialDefs = PartialPartialsFinder.findPartials(partialDefs)
      val partialPartials = partialPartialDefs flatMap (d => index.overridesInClasses(d.symbol) map (s => DefInfo(s, d.nrParamLists, d.nrUntouchableParamLists)))
      AffectedDefs(originals, partials:::partialPartials)
    }

    if(!checkRefactoringParams(prep, affectedDefs, originalParams))
      return Left(RefactoringError("invalid refactoring params for method signature refactoring"))

    def findDef(defdef: DefTree) = filter {
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
        case Apply(fun: Select, _) if fun.hasSymbol && fun.symbol == applySymbol => true
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
      val allDefDefSymbols = affectedDefs.originals
      val allPartialSymbols = affectedDefs.partials
      val singleRefactorings = allDefDefSymbols map (d =>
        refactorDefDef(index.declaration(d.symbol).get, originalParams) &>
        refactorOrdinaryCalls(d.symbol, prepareParamsForSingleRefactoring(originalParams, prep.defdef, d)))
      val singlePartialRefactorings = allPartialSymbols map
        (p => refactorPartialCalls(p.symbol, prepareParamsForSingleRefactoring(originalParams, prep.defdef, p)))
      val refactoring = (singleRefactorings:::singlePartialRefactorings).foldLeft(id[Tree])((t, c) => t &> c)
      refactoring
    }

    val affectedCus = {
      val originalsSymbols = affectedDefs.originals.map(_.symbol)
      val partialsSymbols = affectedDefs.partials.map(_.symbol)
      val allSymbols: List[Symbol] = prep.defdef.symbol::originalsSymbols:::partialsSymbols
      val occurences = allSymbols.flatMap(index.occurences)
      index.rootsOf(occurences)
    }

    val changedTrees = affectedCus flatMap (refactorMethodSignature(_))

    Right(refactor(changedTrees))
  }

  def checkRefactoringParams(selectedValue: PreparationResult, affectedDefs: AffectedDefs, params: RefactoringParameters): Boolean

  def defdefRefactoring(params: RefactoringParameters): Transformation[Tree, Tree]

  def applyRefactoring(params: RefactoringParameters): Transformation[Tree, Tree]

  def paramListPos(fun: Option[Tree]): Int = fun match {
    case Some(Apply(Select(qualifier, _), _)) => 1 + paramListPos(Some(qualifier))
    case Some(Apply(apply: Apply, _)) => 1 + paramListPos(Some(apply))
    case _ => 0
  }

  def traverseApply(t: => Transformation[X, X]) = topdown(t)

  def prepareParamsForSingleRefactoring(originalParams: RefactoringParameters, selectedMethod: DefDef, toRefactor: DefInfo): RefactoringParameters = originalParams

}