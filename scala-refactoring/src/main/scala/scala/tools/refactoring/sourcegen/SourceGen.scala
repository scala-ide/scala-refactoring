package scala.tools.refactoring
package sourcegen

import common.{Change, PimpedTrees}

trait SourceGen extends PimpedTrees with PrettyPrinter {
  
  self: LayoutHelper =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
 
  val transformations = new Transformations[Tree]
  import transformations._
  
  val isModified = filter {
    case EmptyTree => false
    case t: Tree => t.pos == NoPosition || t.pos.isRange
  }

  def generateSourceCode(self: transformations.Transformation[Tree, String], t: Tree): String = t.pos match {
    case NoPosition => 
      prettyPrintTree(self, t)
    case _ =>
      prettyPrintTree(self, t)
  }
  
  def generate(tree: Tree) = {
    recursively(isModified)(generateSourceCode)(tree) 
  }
}