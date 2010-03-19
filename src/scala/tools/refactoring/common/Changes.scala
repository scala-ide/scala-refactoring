package scala.tools.refactoring.common

import scala.tools.nsc.io.AbstractFile
trait Changes {
  
  val global: scala.tools.nsc.Global
  
  // XXX rename to TreeModifications?
  trait TreeChanges {
    
    /*
     * Provides a list of non-overlapping trees that contain changed trees.
     * */
    def toplevelTrees: List[global.Tree]

    def allChangedTrees: List[global.Tree]
  }
}