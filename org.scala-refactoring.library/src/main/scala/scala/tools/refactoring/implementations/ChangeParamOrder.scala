package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change

abstract class ChangeParamOrder extends MethodSignatureRefactoring {

  import global._
  
  type Permutation = List[Int]
  type RefactoringParameters = List[Permutation]
  
  override def checkRefactoringParams(selectedValue: PreparationResult, params: RefactoringParameters) = 
    (selectedValue.vparamss corresponds params) (_.length == _.length)
  
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
    
}