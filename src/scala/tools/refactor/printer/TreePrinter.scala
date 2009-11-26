package scala.tools.refactor.printer

import scala.tools.nsc.ast.Trees

trait TreePrinter {
  
  self: scala.tools.refactor.Compiler =>
  
  import compiler._

  def print(part: WithTree) = part match {
    case part: WithRequisite => printTree(part.tree) copyRequirements part
    case _ => printTree(part.tree)
  }
  
  private def printTree(tree: Trees#Tree) = tree match {
    case DefDef(_, name, _, _, _, _) => StringPart(name.toString)// requirePre("def ") requirePost(" = ") 
    case ValDef(_, name, _, _) => StringPart(name.toString)// requirePre("val ") requirePost(" = ") 
    case Literal(_) => StringPart(tree.toString)
    case tree: TypeTree => StringPart(tree.tpe.toString)
    case Select(qualifier, name) => StringPart(name.toString)
    case _ => throw new Exception("don't know how to create "+ tree.getClass)
  }
}
