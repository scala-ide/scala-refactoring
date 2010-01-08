package scala.tools.refactoring.transformation

import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

private[refactoring] trait Transform {
  
  val global: scala.tools.nsc.Global
  import global._
   
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
  
  def replaceTrees[T](from: List[T], what: List[T], replacement: List[T]): List[T] = {
    if(!from.contains(what.head))
      return from
    
    val (keep1, rest) = from span what.head.!=
    val (_, keep2) = rest span what.contains
    keep1 ::: replacement ::: keep2
  }
  
  // TODO remove
  def reverseClassParameters(t: Tree) = transform(t) {
    case tree @ Template(parents, self, body) => new Template(parents, self, body.reverse)
      .copyAttrs(tree)
  }
}
