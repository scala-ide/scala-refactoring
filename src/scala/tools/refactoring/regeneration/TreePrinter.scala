package scala.tools.refactoring.regeneration

import scala.tools.nsc.ast.Trees

trait TreePrinter {
  self: scala.tools.refactoring.util.Tracing with Fragments =>
  
  val global: scala.tools.nsc.interactive.Global  
  import global._

  def renderTree(part: WithTree) = printTree(part.tree)
  
  private def printTree(tree: Trees#Tree) = context("print tree") { 
    returns(tree match {
      case DefDef(_, name, _, _, _, _) => name.toString
      
      case ValDef(_, name, _, _) => name.toString
      
      case Literal(_) => tree.toString

      case tree: TypeTree if tree.tpe != null => tree.tpe match {
        case tpe if tpe == EmptyTree.tpe => ""
        case tpe: ConstantType => tpe.underlying.toString
        case _ => tree.tpe.toString
      }
              
      case Select(qualifier, name) => name.toString
      
      case Ident(name) => name.toString
      
      case t: If => "if"
      
      case t: Bind => t.name.toString
      
      case _ => throw new Exception("don't know how to create "+ tree.getClass +" for AST "+ tree)
    }) { f: String =>
      trace("printing tree (%s) %s → %s", tree.getClass, tree, f)
    }
  }
}
