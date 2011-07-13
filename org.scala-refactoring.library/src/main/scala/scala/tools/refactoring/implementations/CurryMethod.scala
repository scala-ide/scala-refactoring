package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change

abstract class CurryMethod extends MultiStageRefactoring with common.InteractiveScalaCompiler {

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
        (vparamsRanges zip sectionRanges).foldLeft(true)((b, t) => b && (t._1 containsSlice t._2))
      }
    }
    
    if(!checkRefactoringParams(selectedValue.vparamss, params))
      return Left(RefactoringError("invalid split positions argument for selected method"))
      
    val findDef = filter {
      case d: DefDef => d == selectedValue
    }
    
    def currySingleDefDefParamList(origVparams: List[ValDef], positions: SplitPositions, lastPosition: Int = 0): List[List[ValDef]] = positions match {
      case Nil => List(origVparams)
      case pos::poss => {
        val (start, rest) = origVparams.splitAt(pos - lastPosition)
        start::currySingleDefDefParamList(rest, poss, pos)
      }
    }
    
    def curryDefDefParamLists(origVparamss: List[List[ValDef]], positionss: List[SplitPositions]) = {
      (origVparamss zip positionss).flatMap(l => currySingleDefDefParamList(l._1, l._2))
    }
    
    val curryParamLists = transform {
      case orig @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
        val curried = curryDefDefParamLists(vparamss, params)
        DefDef(mods, name, tparams, curried, tpt, rhs) replaces orig
      }
    }
    
    val curry = topdown {
      matchingChildren {
        findDef &> curryParamLists
      }
    }
    
    Right(transformFile(selection.file, curry))
  }
}