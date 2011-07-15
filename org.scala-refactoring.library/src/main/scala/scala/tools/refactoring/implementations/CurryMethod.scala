package scala.tools.refactoring
package implementations

import common.Change
import common.PimpedTrees

abstract class CurryMethod extends MultiStageRefactoring with common.InteractiveScalaCompiler with PimpedTrees {

  import global._
  
  type PreparationResult = DefDef
  
  type SplitPositions = List[Int]
  type RefactoringParameters = List[SplitPositions]
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[DefDef] match {
      case Some(defdef) => Right(defdef)
      case None => Left(new PreparationError("no defdef selected"))
    }
  }
  
  override def perform(selection: Selection, selectedValue: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    def checkRefactoringParams(vparamss: List[List[ValDef]], sectionss: List[SplitPositions]): Boolean = {
      val sortedSections = sectionss.map(Set(_: _*).toList.sorted)
      if(sortedSections != sectionss || vparamss.size != sectionss.size) {
        false
      } else {
        val sectionRanges = sectionss.map {case Nil => 1 to 0 ; case s => s.head to s.last}
        val vparamsRanges = vparamss.map(1 until _.size)
        (vparamsRanges zip sectionRanges).foldLeft(true)((b, ranges) => b && (ranges._1 containsSlice ranges._2))
      }
    }
    
    if(!checkRefactoringParams(selectedValue.vparamss, params))
      return Left(RefactoringError("invalid split positions argument for selected method"))
      
    def currySingleParamList[T](origVparams: List[T], positions: SplitPositions, lastPosition: Int = 0): List[List[T]] = positions match {
      case Nil => List(origVparams)
      case pos::poss => {
        val (start, rest) = origVparams.splitAt(pos - lastPosition)
        start::currySingleParamList(rest, poss, pos)
      }
    }
    
    def paramListPos(fun: Option[Tree]): Int = fun match {
      case Some(Apply(f, _)) => 1 + paramListPos(Some(f))
      case _ => 0
    }
    
    def makeCurriedApply(baseFun: Tree, vparamss: List[List[Tree]]): Apply = vparamss match {
      case p::Nil => Apply(baseFun, p)
      case x::xs => makeCurriedApply(Apply(baseFun, x), xs)
      case _ => throw new IllegalArgumentException("can't handle empty vparamss")
    }
    
    val findDef = filter {
      case d: DefDef => d == selectedValue
    }
    
    val curryParamsDefDef = transform {
      case orig @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
        val curried = (vparamss zip params) flatMap (l => currySingleParamList(l._1, l._2))
        DefDef(mods, name, tparams, curried, tpt, rhs) replaces orig
      }
    }
    
    val curryDefinition = topdown {
      matchingChildren {
        findDef &> curryParamsDefDef
      }
    }
    
    val findApply = filter {
      case apply: Apply => apply.symbol.fullName == selectedValue.symbol.fullName
    }
    
    val curryParamsApply = transform {
      case orig @ Apply(fun, args) => {
        val pos = paramListPos(findOriginalTree(orig)) - 1
        val curriedParamLists = currySingleParamList(orig.args, params(pos))
        makeCurriedApply(fun, curriedParamLists) replaces orig
      }
    }
    
    val curryCalls = bottomup {
      matchingChildren {
        findApply &> curryParamsApply
      }
    }
    
    Right(transformFile(selection.file, curryDefinition &> curryCalls))
  }
}