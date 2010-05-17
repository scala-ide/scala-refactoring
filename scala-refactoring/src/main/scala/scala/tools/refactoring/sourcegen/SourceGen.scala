package scala.tools.refactoring
package sourcegen

import common.{PimpedTrees, Tracing}

trait SourceGen extends PrettyPrinter with ReusingPrinter with PimpedTrees with LayoutHelper with Formatting {
  
  this: Tracing =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def generate(tree: Tree): String = {
    
    def generateSourceCode(t: Tree, ind: Indentation): Option[String] = {
      
      if(t == null || t.isEmpty)
        None
      else if(t.pos == NoPosition)
        Some(prettyPrintTree(generateSourceCode, t, ind))
      else if (t.pos.isRange)
        Some(reuseExistingSource(generateSourceCode, t, ind))
      else 
        None
    }

    val in = new Indentation(defaultIncrement = defaultIndentationStep)
    val initialIndentation = if(tree.pos != NoPosition) indentation(tree) else ""
    
    in.setTo(initialIndentation) {
      generateSourceCode(tree, in) getOrElse ""
    }
  }
}