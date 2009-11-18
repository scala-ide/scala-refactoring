package scala.tools.refactor.printer

import scala.tools.nsc.ast.Trees

trait TreePrinter {
  
  self: scala.tools.refactor.Compiler =>
  
  import compiler._

  def print(part: WithTree) = part match {
    case part: WithRequirement => printTree(part.tree) copyRequirements part
    case _ => printTree(part.tree)
  }
  
  private def printTree(tree: Trees#Tree) = tree match {
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) => StringPart(name.toString) requirePre("def ") requirePost(" = ") 
  }
}
