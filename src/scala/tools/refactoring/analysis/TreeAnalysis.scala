package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees

trait TreeAnalysis {

  def references(ts: List[Trees#Tree]) = {
    Nil
  }
  
}
