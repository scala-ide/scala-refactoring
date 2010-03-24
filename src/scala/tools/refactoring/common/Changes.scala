package scala.tools.refactoring.common

import scala.tools.nsc.io.AbstractFile
// XXX rename
trait Changes {
  
  val global: scala.tools.nsc.Global
  
  trait TreeModifications {
    
    /*
     * Provides a list of non-overlapping trees that contain changed trees.
     * */
    def toplevelTrees: List[global.Tree]

    def allChangedTrees: List[global.Tree]
  }
}