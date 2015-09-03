package scala.tools.refactoring
package implementations

/**
 * Refactoring to merge parameter lists of a method.
 */
abstract class MergeParameterLists extends MethodSignatureRefactoring {

  import global._

  type MergePositions = List[Int]
  type RefactoringParameters = MergePositions

  override def checkRefactoringParams(prep: PreparationResult, affectedDefs: AffectedDefs, params: RefactoringParameters) = {
    val selectedDefDef = prep.defdef
    val allowedMergeIndexesRange = 1 until selectedDefDef.vparamss.size
    val isNotEmpty = (p: RefactoringParameters) => !p.isEmpty
    val isSorted = (p: RefactoringParameters) => (p sortWith (_ < _)) == p
    val uniqueIndexes = (p: RefactoringParameters) => p.distinct == p
    val indexesInRange = (p: RefactoringParameters) => allowedMergeIndexesRange containsSlice (p.head to p.last)
    val mergeable = (p: RefactoringParameters) => {
      val allAffectedDefs = affectedDefs.originals:::affectedDefs.partials
      val preparedParams = allAffectedDefs.map(prepareParamsForSingleRefactoring(params, selectedDefDef, _))
      preparedParams.filter(_ contains 0).isEmpty
    }
    val allConditions = List(isNotEmpty, isSorted, uniqueIndexes, indexesInRange, mergeable)
    allConditions.foldLeft(true)((b, f) => b && f(params))
  }

  override def defdefRefactoring(params: RefactoringParameters) = transform {
    case orig @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
      val vparamssWithIndex = vparamss.zipWithIndex
      val mergedVparamss = vparamssWithIndex.foldLeft(Nil: List[List[ValDef]])((acc, current) => current match {
        case (_, index) if params contains index => (acc.head:::current._1)::acc.tail
        case _ => current._1::acc
      }).reverse
      DefDef(mods, name, tparams, mergedVparamss, tpt, rhs) replaces orig
    }
  }

  override def applyRefactoring(params: RefactoringParameters) = transform {
    case apply @ Apply(fun, args) => {
      val originalTree = findOriginalTree(apply)
      val pos = paramListPos(originalTree) - 1
      if(params contains pos) {
        fun match {
          case Apply(ffun, fargs) => Apply(ffun, fargs:::args)
          case Select(Apply(ffun, fargs), name) => Apply(ffun, fargs:::args)
          case _ => apply
        }
      } else {
        apply
      }
    }
  }

  override def traverseApply(t: ⇒ Transformation[X, X]) = bottomup(t)

  override def prepareParamsForSingleRefactoring(originalParams: RefactoringParameters, selectedMethod: DefDef, toRefactor: DefInfo): RefactoringParameters = {
    val originalNrParamLists = selectedMethod.vparamss.size
    val currentNrParamLists = toRefactor.nrParamLists
    val untouchables = toRefactor.nrUntouchableParamLists
    val toShift = originalNrParamLists - currentNrParamLists - untouchables
    originalParams.map(_ - toShift).filter(_ >= untouchables)
  }
}