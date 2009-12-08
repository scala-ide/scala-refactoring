package scala.tools.refactoring

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.util.RangePosition

trait Selections {
  
  self: scala.tools.refactoring.Compiler =>
  
  import global._
  
  private class FilterTree(start: Int, end: Int) extends Traverser {
    
    val hits = new ListBuffer[Tree]
    
    override def traverse(t: Tree) {
      if (t.pos.isRange && t.pos.start >= start && t.pos.end < end)
        hits += t
      super.traverse(t)
    }
  }
  
  case class TreeSelection(root: Tree, start: Int, end: Int) {
    
    lazy val pos = new RangePosition(root.pos.source, start, start, end)
    
    lazy val trees: List[Tree] = {
      val f = new FilterTree(start, end)
      f.traverse(root)
      f.hits.toList
    }
    
    lazy val symbols = trees flatMap {
      case t: SymTree => Some(t.symbol)
      case _ => None
    }
    
    def contains(t: Tree) = t.pos.source == root.pos.source && pos.includes(t.pos)
  }
}