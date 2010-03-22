package scala.tools.refactoring.common

import scala.tools.nsc.io.AbstractFile
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.util.RangePosition

trait Selections {
  
  val global: Global
  import global._
  import PartialFunction._
  
  trait Selection {
    
    val pos: RangePosition
    
    // a tree that encloses the complete position
    val root: Tree
    
    def file: AbstractFile
    
    lazy val selectedTopLevelTrees: List[Tree] = {
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
     
    lazy val selectedSymbols = allSelectedTrees flatMap {
      case t: SymTree => Some(t.symbol)
      case _ => None
    }
    
    def contains(t: Tree) = isPosContainedIn(t.pos, pos)
    def isContainedIn(t: Tree) = isPosContainedIn(pos, t.pos)
    
    lazy val selectedSymbolTree = (root filter (cond(_) { case t: SymTree => contains(t) }) match {
      case (x: SymTree) :: _ => Some(x)
      case _ => None
    }) orElse findSelectedOfType[SymTree]
    
    def findSelectedOfType[T](implicit m: Manifest[T]) = root filter (cond(_) {
      case t => m.erasure.isInstance(t) && isPosContainedIn(pos, t.pos)
    }) reverse match {
      case x :: _ => Some(x.asInstanceOf[T])
      case _ => None
    }
    
    private[refactoring] lazy val allSelectedTrees: List[Tree] = {
      selectedTopLevelTrees flatMap (_ filter (t => t.pos.isRange && pos.includes(t.pos)))
    }
    
    private def isPosContainedIn(p1: Position, p2: Position) = 
      p1.isRange && !p1.isTransparent && p2.isRange && !p2.isTransparent && p2.includes(p1) && p1.source == p2.source
  }
  
  class FileSelection(val file: AbstractFile, from: Int, to: Int) extends Selection {
    
    lazy val pos = new RangePosition(root.pos.source, from, from, to)
    
    lazy val root = global.unitOfFile(file).body
  }
  
  class TreeSelection(val root: Tree, val pos: RangePosition) extends Selection {
 
    def this(root: Tree, start: Int, end: Int) = this(root, new RangePosition(root.pos.source, start, start, end))
    
    def this(root: Tree) = this(root, if(root.pos.isRange) root.pos.asInstanceOf[RangePosition] else throw new Exception("Position not a range."))
    
    lazy val file = pos.source.file
  }
}
