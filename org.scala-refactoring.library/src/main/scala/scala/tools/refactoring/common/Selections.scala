/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile
import collection.mutable.ListBuffer
import tools.nsc.interactive.Global
import tools.nsc.util.RangePosition

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
      case t => m.erasure.isInstance(t) && isContainedIn(t)
    }) reverse match {
      case x :: _ => Some(x.asInstanceOf[T])
      case _ => None
    }
    
    private[refactoring] lazy val allSelectedTrees: List[Tree] = {
      selectedTopLevelTrees flatMap (_ filter (t => t.pos.isRange && pos.includes(t.pos)))
    }
    
    private def isPosContainedIn(p1: Position, p2: Position) = 
      p1.isRange && 
      !p1.isTransparent && 
      p2.isRange && 
      !p2.isTransparent && 
      p2.includes(p1) && 
      p1.source == p2.source &&
      p1.start < p1.end // some ranges don't have a visible representation
  }
  
  case class FileSelection(val file: AbstractFile, from: Int, to: Int) extends Selection {
    
    lazy val pos = new RangePosition(root.pos.source, from, from, to)
    
    lazy val root = global.unitOfFile(file).body
  }
  
  case class TreeSelection(val root: Tree) extends Selection {
    
    if(!root.pos.isRange)
      error("Position not a range.")
      
    val pos = root.pos.asInstanceOf[RangePosition]
    
    val file = pos.source.file
  }
}
