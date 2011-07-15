package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change

abstract class ChangeParamOrder extends MultiStageRefactoring with common.InteractiveScalaCompiler with analysis.Indexes {

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
    
    def reorder[T](origVparams: List[List[T]], permutations: List[Permutation]): List[List[T]] = origVparams match {
      case Nil => Nil
      case x::xs => reorderSingleParamList(x, permutations.head)::reorder(xs, permutations.tail)
    }
    
    def reorderSingleParamList[T](origVparams: List[T], permutation: Permutation): List[T] = permutation match {
      case Nil => Nil
      case x::xs => origVparams(x)::reorderSingleParamList(origVparams, xs)
    }
    
    def findDef(defdef: DefTree)= filter {
      case d: DefDef => d == defdef
    }
    
    val reorderParams = transform {
      case orig @ DefDef(mods, name, tparams, vparams, tpt, rhs) => {
        val reorderedVparams = reorder(vparams, params)
        DefDef(mods, name, tparams, reorderedVparams, tpt, rhs) replaces orig
      }
    }
    
    def changeParamOrderDefDef(defdef: DefTree) = topdown {
      matchingChildren {
        findDef(defdef) &> reorderParams
      }
    }
    
    def findApply(applySymbol: Symbol) = filter {
      case apply: Apply => apply.symbol.fullName == applySymbol.fullName
    }
    
    def paramListPos(fun: Option[Tree]): Int = fun match {
      case Some(Apply(f, _)) => 1 + paramListPos(Some(f))
      case _ => 0
    }

    val reorderParamsApply = transform {
      case orig @ Apply(fun, args) => {
        val pos = paramListPos(findOriginalTree(orig)) - 1
        val reorderedArgs = reorderSingleParamList(args, params(pos))
        Apply(fun, reorderedArgs) replaces orig
      }
    }
    
    def changeParamOrderApply(applySymbol: Symbol) = topdown {
      matchingChildren {
        findApply(applySymbol) &> reorderParamsApply
      }
    }
    
    def findUsages(defdef: DefTree)  {
      println(index.references(defdef.symbol))
    }
    
    def changeParamOrder(defdef: DefDef) = {
      val allDefDefs = index.overridesInClasses(defdef.symbol)
      val defs = index.allDefinedSymbols filter(_.isMethod) flatMap (index.declaration)
      val singleReorderings = allDefDefs map (d => changeParamOrderDefDef(index.declaration(d).get) &> changeParamOrderApply(d))
      singleReorderings.foldLeft(id[Tree])((t, c) => t &> c)
    }
    
    Right(transformFile(selection.file, changeParamOrder(selectedValue)))
  }
  
}