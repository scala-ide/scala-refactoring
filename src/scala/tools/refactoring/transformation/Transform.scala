package scala.tools.refactoring.transformation

import scala.tools.nsc.io.AbstractFile
import scala.collection.mutable.HashSet
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags
import scala.tools.refactoring.common.Changes

private[refactoring] trait Transform extends Changes {
  outer =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def transform(root: Tree, changed: Tree => Unit = (_ => ()))(trans: PartialFunction[Tree, Tree]): Tree = {
    new Transformer {
      override def transform(tree: Tree): Tree = {
        val result = super.transform {
          if(trans.isDefinedAt(tree)) {
            trans(tree)
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
  
  trait ChangeCollector extends TreeChanges {
    
    def transform(root: Tree) = outer.transform(root, (changes ::= _)) _
    
    def toplevelTrees = topChanges
  
    def allChangedTrees = changes

    private var changes = Nil: List[Tree]
    
    private def topChanges = {
      
      def findSuperTrees(trees: List[Tree], superTrees: List[Tree]): List[Tree] = trees match {
        case Nil => superTrees
        case t :: ts =>
        
          def mergeOverlappingTrees(ts: List[Tree]): List[Tree] = ts match {
            case Nil => t :: Nil
            case x :: xs if x.pos properlyIncludes t.pos => x :: xs
            case x :: xs if t.pos properlyIncludes x.pos => t :: xs
            case x :: xs => x :: mergeOverlappingTrees(xs)
          }
        
          findSuperTrees(ts, mergeOverlappingTrees(superTrees))
      }
      
      findSuperTrees(changes, Nil)
    }
  }
  
  implicit def blockToTreeList(b: Block) = b.stats ::: b.expr :: Nil
  
  implicit def abstractFileToTree(file: AbstractFile): global.Tree = global.unitOfFile(file).body
    
  def replace[T](from: List[T], what: List[T], replacement: List[T]): List[T] = (from, what) match {
    case (Nil, _) => Nil
    case (xs, Nil) => xs
    case (x :: xs, y :: ys) if x == y => replacement ::: replace(xs, ys, Nil)
    case (x :: xs, _) => x :: replace(xs, what, replacement)
  }
}
