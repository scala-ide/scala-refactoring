package scala.tools.refactoring.common

trait Changes {
  
  val global: scala.tools.nsc.Global
  
  case class Change(from: Int, to: Int, text: String)
  
  type ChangeSet = List[Change]
  
  trait TreeChanges {
    
    /*
     * Provides a list of non-overlapping trees that contain changed trees.
     * */
    def toplevelTrees: List[global.Tree]

    def allChangedTrees: List[global.Tree]
  }
}