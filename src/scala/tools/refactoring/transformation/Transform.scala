package scala.tools.refactoring.transformation

import scala.tools.refactoring.UnknownPosition
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

private[refactoring] trait Transform {
  
  val global: scala.tools.nsc.Global
  import global._
  
  private class PositionSetter(guard: Tree => Boolean) extends Traverser {
     override def traverse(t: Tree): Unit = {
       if(guard(t)) {
         t setPos UnknownPosition
       }
       super.traverse(t)
     }
  }
  
  private object noPositionSetter  extends PositionSetter( _.pos == NoPosition )
  private object allPositionSetter extends PositionSetter( _ => true )
  
  def cleanAll[T <: Tree](body: => T): T = {
    val tree: T = body
    allPositionSetter.traverse(tree)
    tree
  }
  
  def cleanNoPos[T <: Tree](body: => T): T = {
    val tree: T = body
    noPositionSetter.traverse(tree)
    tree
  }
  
  def transform(root: Tree)(body: PartialFunction[Tree, Tree]): Tree = new Transformer {
    override def transform(tree: Tree): Tree = {
      val res = super.transform(tree)
      if(body.isDefinedAt(res)) {
        body(res)
      } else { 
        res
      }
    }
  }.transform(root)
  
  def replaceTrees(from: List[Tree], what: List[Tree], through: Tree) = {
    val (keep1, rest) = from break what.head.==
    val (_, keep2) = rest span what.contains
    keep1 ::: through :: keep2
  }
  
  def reverseClassParameters(t: Tree) = transform(t) {
    case tree @ Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
  }
}
