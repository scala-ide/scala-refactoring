package scala.tools.refactoring.transformation

import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

private[refactoring] trait Transform {
  
  val global: scala.tools.nsc.Global
  import global._
   
  def transform(root: Tree)(trans: Tree =>? Tree): Tree = new Transformer {
    override def transform(tree: Tree): Tree = super.transform {
      if(trans.isDefinedAt(tree)) {
        trans(tree) setPos tree.pos
      } else { 
        tree
      }
    }
  }.transform(root)
  
  
  def replaceTrees(b: Block, what: List[Tree], replacement: List[Tree]): List[Tree] = 
    replaceTrees(b.stats ::: b.expr :: Nil, what, replacement)
  
  def replaceTrees[T](from: List[T], what: List[T], replacement: List[T]): List[T] = {
    if(!from.contains(what.head))
      return from
    
    val (keep1, rest) = from span what.head.!=
    val (_, keep2) = rest span what.contains
    keep1 ::: replacement ::: keep2
  }
  
  // TODO remove
  def reverseClassParameters(t: Tree) = transform(t) {
    case tpl: Template => tpl.copy(body = tpl.body.reverse)
  }
}
