package scala.tools.refactoring.sourcegen

import tools.nsc.util.RangePosition

trait LayoutHelper {
  
  self: Formatting =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  //def findOriginalParent(t: Tree): Tree
 
  def leftLayout(t: Tree, default: String): Layout = t.pos match {
    case NoPosition => 
      new LayoutFromString(default)
    case pos: RangePosition => 
      //findOriginalParent(t)
      new LayoutFromString(default)
  }
  
  def rightLayout(t: Tree, default: String): Layout = {
    new LayoutFromString(default)
  }
}