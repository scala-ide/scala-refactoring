package scala.tools.refactoring.regeneration

import scala.tools.refactoring.regeneration.Fragment
import scala.tools.nsc.ast.Trees

trait TreePrinter {
  self: scala.tools.refactoring.util.Tracing =>
  val global: scala.tools.nsc.Global  
  import global._

  def print(part: WithTree) = part match {
    case part: WithRequisite => printTree(part.tree) copyRequirements part
    case _ => printTree(part.tree)
  }
  
  private def printTree(tree: Trees#Tree) = context("printTree") { 
    using(tree match {
      case DefDef(_, name, _, _, _, _) => StringFragment(name.toString)// requirePre("def ") requirePost(" = ") 
      case ValDef(_, name, _, _) => StringFragment(name.toString)// requirePre("val ") requirePost(" = ") 
      case Literal(_) => StringFragment(tree.toString)
      case tree: TypeTree if tree.tpe == EmptyTree.tpe =>  StringFragment("")
      case tree: TypeTree if tree.tpe != null => StringFragment(tree.tpe.toString)
      case Select(qualifier, name) => StringFragment(name.toString)
      case Ident(name) => StringFragment(name.toString)
      case _ => throw new Exception("don't know how to create "+ tree.getClass +" for AST "+ tree)
    }) { f: StringFragment =>
      trace("printing tree (%s) %s → %s", tree.getClass, tree, f)
    }
  }
}
