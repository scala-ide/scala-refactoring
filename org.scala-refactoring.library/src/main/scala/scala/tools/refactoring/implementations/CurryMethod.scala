package scala.tools.refactoring
package implementations

import common.Change
import common.PimpedTrees

abstract class CurryMethod extends MethodSignatureRefactoring {

  import global._
  
  type SplitPositions = List[Int]
  type RefactoringParameters = List[SplitPositions]
  
  override def checkRefactoringParams(selectedValue: PreparationResult, params: RefactoringParameters) = {
    def checkRefactoringParamsHelper(vparamss: List[List[ValDef]], sectionss: List[SplitPositions]): Boolean = {
      val sortedSections = sectionss.map(Set(_: _*).toList.sorted)
      if(sortedSections != sectionss || vparamss.size != sectionss.size) {
        false
      } else {
        val sectionRanges = sectionss.map {case Nil => 1 to 0 ; case s => s.head to s.last}
        val vparamsRanges = vparamss.map(1 until _.size)
        (vparamsRanges zip sectionRanges).foldLeft(true)((b, ranges) => b && (ranges._1 containsSlice ranges._2))
      }
    }
    
    checkRefactoringParamsHelper(selectedValue.vparamss, params)
  }
  
  def currySingleParamList[T](origVparams: List[T], positions: SplitPositions, lastPosition: Int = 0): List[List[T]] = positions match {
    case Nil => List(origVparams)
    case pos::poss => {
      val (start, rest) = origVparams.splitAt(pos - lastPosition)
      start::currySingleParamList(rest, poss, pos)
    }
  }
    
  def makeCurriedApply(baseFun: Tree, vparamss: List[List[Tree]]): Apply = vparamss match {
    case p::Nil => Apply(baseFun, p)
    case x::xs => makeCurriedApply(Apply(baseFun, x), xs)
    case _ => throw new IllegalArgumentException("can't handle empty vparamss")
  }
    
  override def defdefRefactoring(params: RefactoringParameters) = transform {
    case orig @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
      val curried = (vparamss zip params) flatMap (l => currySingleParamList(l._1, l._2))
      DefDef(mods, name, tparams, curried, tpt, rhs) replaces orig
    }
  }
    
  override def applyRefactoring(params: RefactoringParameters) = transform {
    case orig @ Apply(fun, args) => {
      val pos = paramListPos(findOriginalTree(orig)) - 1
      val curriedParamLists = currySingleParamList(orig.args, params(pos))
      makeCurriedApply(fun, curriedParamLists) replaces orig
    }
  }
    
  override def traverseApply[X <% (X ⇒ X) ⇒ X](t: ⇒ Transformation[X, X]) = bottomup(t)
}