/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.regeneration

import scala.tools.nsc.ast.Trees

trait TreePrinter {
  self: scala.tools.refactoring.common.Tracing with Fragments =>
  
  val global: scala.tools.nsc.interactive.Global  
  import global._

  def renderTree(part: WithTree) = printTree(part.tree)
  
  private def printTree(tree: Trees#Tree) = context("print tree") { 
    (tree match {
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
      
      case t: Bind => t.name.toString
      
      case c: ClassDef => c.name.toString
      
      case m: ModuleDef => m.name.toString
      
      case t: TypeDef => t.name.toString
      
      case t: This => "this"
              
      case t: If => "if"
      
      case _ => throw new Exception("don't know how to create "+ tree.getClass +" for AST "+ tree)
    }) \\ (trace("printing tree (%s) %s → %s", tree.getClass, tree, _))
  }
}
