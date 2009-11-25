package scala.tools.refactor.transform

import scala.tools.refactor.Compiler
import scala.tools.refactor.UnknownPosition
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

trait Transform {
  
  self: scala.tools.refactor.Compiler =>
  import compiler._
  
  private object PositionSetter extends Traverser {
     override def traverse(tree: Tree): Unit = {
       
       if(tree.pos == NoPosition) {
         tree setPos UnknownPosition
       }

       super.traverse(tree)
     }
  }
  
  def cleanTree[T <: Tree](body: => T): T = {
    val tree: T = body
    PositionSetter.traverse(tree)
    tree
  }
  
  def reverseClassParameters = new Transformer {
    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
      case x => x
    }
  }

}
