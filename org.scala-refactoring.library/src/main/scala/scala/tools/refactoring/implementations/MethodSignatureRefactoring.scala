package scala.tools.refactoring
package implementations

import common.Change
import common.PimpedTrees

abstract class MethodSignatureRefactoring extends MultiStageRefactoring with common.InteractiveScalaCompiler with PimpedTrees with analysis.Indexes {

  import global._
  
  type PreparationResult = DefDef
  
  def prepare(s: Selection) = s.findSelectedOfType[DefDef] toRight PreparationError("no defdef selected")
  
  override def perform(selection: Selection, selectedValue: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    if(!checkRefactoringParams(selectedValue, params))
      return Left(RefactoringError("invalid refactoring params for method signature refactoring"))

    def findDef(defdef: DefTree)= filter {
      case d: DefDef => d == defdef
    }
      
    def refactorDefDef(defdef: DefTree) = topdown {
      matchingChildren {
        findDef(defdef) &> defdefRefactoring(params)
      }
    }
    
    def findApply(applySymbol: Symbol) = filter {
      case apply: Apply => apply.symbol.fullName == applySymbol.fullName
    }
    
    def refactorCalls(applySymbol: Symbol) = traverseApply {
      matchingChildren {
        findApply(applySymbol) &> applyRefactoring(params)
      }
    }
    
    val refactorMethodSignature = {
      val allDefDefs = index.overridesInClasses(selectedValue.symbol)
      val singleRefactorings = allDefDefs map (d => refactorDefDef(index.declaration(d).get) &> refactorCalls(d))
      singleRefactorings.foldLeft(id[Tree])((t, c) => t &> c)
    }
    
    Right(transformFile(selection.file, refactorMethodSignature))
  }
  
  def checkRefactoringParams(selectedValue: PreparationResult, params: RefactoringParameters): Boolean
  
  def defdefRefactoring(params: RefactoringParameters): Transformation[Tree, Tree]
  
  def applyRefactoring(params: RefactoringParameters): Transformation[Tree, Tree]
  
  def paramListPos(fun: Option[Tree]): Int = fun match {
      case Some(Apply(f, _)) => 1 + paramListPos(Some(f))
      case _ => 0
  }
    
  def traverseApply[X <% (X ⇒ X) ⇒ X](t: => Transformation[X, X]) = topdown(t)
  
}