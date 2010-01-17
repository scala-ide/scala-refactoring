package scala.tools.refactoring.transformation

import scala.collection.mutable.HashSet
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

private[refactoring] trait Transform {
  
  val global: scala.tools.nsc.Global
  import global._
   
  def transform(root: Tree)(trans: PartialFunction[Tree, Tree]): Tree = {
    new Transformer {
      override def transform(tree: Tree): Tree = {
        super.transform {
          if(trans.isDefinedAt(tree)) {
            val result = trans(tree)
            //if(result.getClass == tree.getClass)
              result setPos tree.pos
            //else
            //  result
          } else {
            tree
          }
        }
      }
    }.transform(root)
  }
  
  implicit def blockToTreeList(b: Block) = b.stats ::: b.expr :: Nil
    
  def replace[T](from: List[T], what: List[T], replacement: List[T]): List[T] = (from, what) match {
    case (Nil, _) => Nil
    case (xs, Nil) => xs
    case (x :: xs, y :: ys) if x == y => replacement ::: replace(xs, ys, Nil)
    case (x :: xs, _) => x :: replace(xs, what, replacement)
  }
}
