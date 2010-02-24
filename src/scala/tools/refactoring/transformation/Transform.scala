package scala.tools.refactoring.transformation

import scala.collection.mutable.HashSet
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

private[refactoring] trait Transform {
  outer =>
  
  val global: scala.tools.nsc.Global
  import global._
  
  def transform(root: Tree, changed: Tree => Unit = (_ => ()))(trans: PartialFunction[Tree, Tree]): Tree = {
    new Transformer {
      override def transform(tree: Tree): Tree = {
        val result = super.transform {
          if(trans.isDefinedAt(tree)) {
            trans(tree) setPos tree.pos
          } else {
            tree
          }
        }
        // emit the changed tree after all sub-transformations have been applied
        if(trans.isDefinedAt(tree) && result != tree) changed(result)
        result
      }
    }.transform(root)
  }
  
  trait Transformation {
    private var changes = Nil: List[Tree]
    def transform(root: Tree) = outer.transform(root, (changes ::= _)) _
    
    /* there is always a single top-level tree that encloses all changed trees.
     * This tree is also always a tree that already exists, if it were a new tree,
     * then it would in turn have a parent.
    */
    def topChange = changes reduceLeft { (t1, t2) =>
      if(t1.pos.properlyIncludes(t2.pos)) t1 else t2
    }
  }
  
  implicit def blockToTreeList(b: Block) = b.stats ::: b.expr :: Nil
    
  def replace[T](from: List[T], what: List[T], replacement: List[T]): List[T] = (from, what) match {
    case (Nil, _) => Nil
    case (xs, Nil) => xs
    case (x :: xs, y :: ys) if x == y => replacement ::: replace(xs, ys, Nil)
    case (x :: xs, _) => x :: replace(xs, what, replacement)
  }
}
