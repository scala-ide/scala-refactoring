package scala.tools.refactoring

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.util.RangePosition

trait Selections {
  
  self: scala.tools.refactoring.Compiler =>
  
  import global._
  
  private class FilterTree(start: Int, end: Int, includeChildren: Boolean) extends Traverser {
    
    val hits = new ListBuffer[Tree]
    
    override def traverse(t: Tree) {
      if (t.pos.isRange && t.pos.start >= start && t.pos.end <= end) {
        hits += t
        if(includeChildren) super.traverse(t)
      } else 
        super.traverse(t)
    }
  }
  
  class TreeSelection(root: Tree, start: Int, end: Int) {
    
    def this(root: Tree) = this(root, root.pos.start, root.pos.end)
    
    lazy val pos = new RangePosition(root.pos.source, start, start, end)
    
    lazy val trees: List[Tree] = {
      val f = new FilterTree(start, end, false)
      f.traverse(root)
      f.hits.toList
    }    
    
    lazy val treesWithSubtrees: List[Tree] = {
      val f = new FilterTree(start, end, true)
      trees foreach f.traverse
      f.hits.toList
    }
 
    lazy val symbols = treesWithSubtrees flatMap {
      case t: SymTree => Some(t.symbol)
      case _ => None
    }
    
    def contains(t: Tree) = t.pos.source == root.pos.source && pos.includes(t.pos)
  }
}