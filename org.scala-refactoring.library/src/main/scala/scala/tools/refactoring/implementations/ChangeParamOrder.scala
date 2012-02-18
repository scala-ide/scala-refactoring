package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change

/**
 * Refactoring that changes the order of the parameters of a method.
 */
abstract class ChangeParamOrder extends MethodSignatureRefactoring {

  import global._
  
  type Permutation = List[Int]
  /**
   * There has to be a permutation for each parameter list of the selected method.
   */
  type RefactoringParameters = List[Permutation]
  
  override def checkRefactoringParams(prep: PreparationResult, affectedDefs: AffectedDefs, params: RefactoringParameters) = 
    (prep.defdef.vparamss corresponds params) ((vp, p) => (0 until vp.length) containsSlice (p.sortWith(_ < _)))
  
  def reorder[T](origVparamss: List[List[T]], permutations: List[Permutation]): List[List[T]] =
    (origVparamss zip permutations) map {
      case (params, perm) => reorderSingleParamList(params, perm)
    }
    
  def reorderSingleParamList[T](origVparams: List[T], permutation: Permutation) = 
    permutation map origVparams
    
  override def defdefRefactoring(parameters: RefactoringParameters) = transform {
    case orig @ DefDef(mods, name, tparams, vparams, tpt, rhs) => {
      val reorderedVparams = reorder(vparams, parameters)
      DefDef(mods, name, tparams, reorderedVparams, tpt, rhs) replaces orig
    }
  }
    
  override def applyRefactoring(params: RefactoringParameters) = transform {
    case orig @ Apply(fun, args) => {
      val pos = paramListPos(findOriginalTree(orig)) - 1
      val reorderedArgs = reorderSingleParamList(args, params(pos))
      Apply(fun, reorderedArgs) replaces orig
    }
  }
  
  override def prepareParamsForSingleRefactoring(originalParams: RefactoringParameters, selectedMethod: DefDef, toRefactor: AffectedDef): RefactoringParameters = {
    val toDrop = originalParams.size - toRefactor.nrParamLists
    originalParams.drop(toDrop)
  }
    
}