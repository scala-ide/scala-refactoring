package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change

abstract class ChangeParamOrder extends MethodSignatureRefactoring {

  import global._
  
  type Permutation = List[Int]
  type RefactoringParameters = List[Permutation]
  
  override def checkRefactoringParams(selectedValue: PreparationResult, params: RefactoringParameters): Boolean = {
    
    def checkRefactoringParamsHelper(vparams: List[List[ValDef]], permutations: List[Permutation]): Boolean = vparams match {
      case Nil => permutations == Nil
      case x::xs => permutations match {
        case Nil => false
        case y::ys => x.length == y.length && checkRefactoringParamsHelper(xs, ys)
      }
    }
    
    checkRefactoringParamsHelper(selectedValue.vparamss, params)
  }
  
  def reorder[T](origVparams: List[List[T]], permutations: List[Permutation]): List[List[T]] = origVparams match {
      case Nil => Nil
      case x::xs => reorderSingleParamList(x, permutations.head)::reorder(xs, permutations.tail)
    }
    
  def reorderSingleParamList[T](origVparams: List[T], permutation: Permutation): List[T] = permutation match {
    case Nil => Nil
    case x::xs => origVparams(x)::reorderSingleParamList(origVparams, xs)
  }
    
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