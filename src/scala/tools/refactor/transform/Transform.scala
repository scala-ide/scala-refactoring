package scala.tools.refactor.transform

import scala.tools.refactor.Compiler
import scala.tools.nsc.util.Position
import scala.tools.nsc.util.RangePosition

trait Transform {
  
  self: scala.tools.refactor.Compiler =>
  import compiler._
  
  import typer.{typed, atOwner}    // methods to type trees

  def reverseClassParameters = new Transformer {
    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
      case x => x
    }
  }
  
  def insertValue = new Transformer {
    override def transform(tree: Tree): Tree = {
      val tree1 = super.transform(tree) 
      tree1 match {
        
        case tpl @ Template(parents, self, body) => 
              
          val v = atPos(tree1.pos)(ValDef(Modifiers(scala.tools.nsc.symtab.Flags.PRIVATE), newTermName("sample"), TypeTree(NoType), EmptyTree)) 
          
          new Template(parents, self, v :: body).copyAttrs(tree)
        
        case x => x
      }
    }
  }
}
