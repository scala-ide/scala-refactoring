package scala.tools.refactoring.util

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.Global
import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.util.RangePosition

trait Selections {
  
  val global: Global
  import global._
  
  class TreeSelection(root: Tree, pos: RangePosition) {
 
    def this(root: Tree, start: Int, end: Int) = this(root, new RangePosition(root.pos.source, start, start, end))
    
    def this(root: Tree) = this(root, if(root.pos.isRange) root.pos.asInstanceOf[RangePosition] else throw new Exception("Position not a range."))
    
    lazy val trees: List[Tree] = {
      val hits = new ListBuffer[Tree]
      new Traverser {
        override def traverse(t: Tree) {
          if (t.pos.isRange && pos.includes(t.pos)) {
            hits += t
          } else 
            super.traverse(t)
        }
      }.traverse(root)
      hits.toList
    }
    
    lazy val treesWithSubtrees: List[Tree] = {
      trees flatMap (_ filter (t => t.pos.isRange && pos.includes(t.pos)))
    }
 
    lazy val symbols = treesWithSubtrees flatMap {
      case t: SymTree => Some(t.symbol)
      case _ => None
    }

    def contains(t: Tree) = t.pos.source == root.pos.source && pos.includes(t.pos)
    
    lazy val enclosingDefDef = findSelectedOfType[DefDef]
    
    import PartialFunction._
   
    lazy val selectedSymbolTree = (root filter (cond(_) { case t: SymTree => this contains t }) match {
      case (x: SymTree) :: _ => Some(x)
      case _ => None
    }) orElse findSelectedOfType[SymTree]
    
    private def findSelectedOfType[T](implicit m: Manifest[T]) = root filter (cond(_) {
      case t => m.erasure.isInstance(t) && this.isContainedIn(t)
    }) reverse match {
      case x :: _ => Some(x.asInstanceOf[T])
      case _ => None
    }
    
    private def isContainedIn(t: Tree) = t.pos.source == root.pos.source && t.pos.includes(pos)
  }
}
