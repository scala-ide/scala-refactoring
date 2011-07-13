package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change

abstract class ChangeParamOrder extends MultiStageRefactoring with common.InteractiveScalaCompiler {

  import global._
  
  type Permutation = List[Int]
  
  type PreparationResult = DefDef
  
  type RefactoringParameters = List[Permutation]
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[DefDef] match {
      case Some(defdef) => Right(defdef)
      case None => Left(new PreparationError("no defdef selected"))
    }
  }
  
  override def perform(selection: Selection, selectedValue: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    def checkRefactoringParams(vparams: List[List[ValDef]], permutations: List[Permutation]): Boolean = vparams match {
      case Nil => permutations == Nil
      case x::xs => permutations match {
        case Nil => false
        case y::ys => x.length == y.length && checkRefactoringParams(xs, ys)
      }
    }
    
    if(!checkRefactoringParams(selectedValue.vparamss, params)) 
      return Left(RefactoringError("wrong length of permutation(s) of method arguments"))
    
    val findDef= filter {
      case d: DefDef => d == selectedValue
    }
    
    def reorder[T](origVparams: List[List[T]], permutations: List[Permutation]): List[List[T]] = origVparams match {
      case Nil => Nil
      case x::xs => reorderSingleParamList(x, permutations.head)::reorder(xs, permutations.tail)
    }
    
    def reorderSingleParamList[T](origVparams: List[T], permutation: Permutation): List[T] = permutation match {
      case Nil => Nil
      case x::xs => origVparams(x)::reorderSingleParamList(origVparams, xs)
    }
    
    val reorderParams = transform {
      case orig @ DefDef(mods, name, tparams, vparams, tpt, rhs) => {
        val reorderedVparams = reorder(vparams, params)
        DefDef(mods, name, tparams, reorderedVparams, tpt, rhs) replaces orig
      }
    }
    
    val changeParamOrderDefDef = topdown {
      matchingChildren {
        findDef &> reorderParams
      }
    }
    
    val findApply = filter {
      case apply: Apply => apply.symbol.fullName == selectedValue.symbol.fullName
    }
    
    // TODO quite a hack => find a cleaner way to do it
    var applyPermutations = params.reverse
    val reorderParamsApply = transform {
      case orig @ Apply(fun, args) => {
        val permutation = applyPermutations.head
        applyPermutations = applyPermutations.tail match {
          case Nil => params.reverse
          case perms => perms
        }
        val reorderedArgs = reorderSingleParamList(args, permutation)
        Apply(fun, reorderedArgs) replaces orig
      }
    }
    
    val changeParamOrderApply = topdown {
      matchingChildren {
        findApply &> reorderParamsApply
      }
    }
    
    Right(transformFile(selection.file, changeParamOrderDefDef &> changeParamOrderApply))
  }
  
}