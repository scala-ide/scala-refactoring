package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile

trait PimpedTrees {
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def treeForFile(file: AbstractFile): Option[Tree]
    
  // clean the tree here
  def cuRoot(p: Position) = treeForFile(p.source.file)
  
  object ModifiersTree {
    def unapply(m: global.Modifiers) = {
      if(m.positions.isEmpty)
        None
      else
        Some(m.positions.toList map (m => ModifierTree(m._1, m._2 withEnd (m._2.end + 1))) sortBy (_.pos.start))
    }
  }
  
  case class NameTree(name: Name, override val pos: Position) extends Tree {
    setPos(pos)
  }
  
  case class ModifierTree(flag: Long, override val pos: Position) extends Tree {
    setPos(pos)
  }
  
  implicit def pimpPositions(t: Tree) = new {
    def samePos(p: Position): Boolean = t.pos.sameRange(p) && t.pos.source == p.source
    def samePos(o: Tree)    : Boolean = samePos(o.pos)
    def namePosition: Position = t match {
      case t: ClassDef => t.pos withStart (t.pos.point) withEnd (t.pos.point + t.name.toString.trim.length)
      case t: ValDef   => t.pos withStart (t.pos.point) withEnd (t.pos.point + t.name.toString.trim.length)
      
      case _ => NoPosition
    }
  }
  
  
  
  implicit def treeToPimpedTree(t: Tree) = new {
    def originalParent       = cuRoot(t.pos) flatMap (_ find (_.children exists (_ samePos t)))
    def originalLeftSibling  = findSibling(originalParent, 1, 0)
    def originalRightSibling = findSibling(originalParent, 0, 1)
    private def findSibling(parent: Option[Tree], compareIndex: Int, returnIndex: Int) = 
      parent flatMap (_.children filter (_.pos.isRange) sliding 2 find (_ lift compareIndex map (_ samePos t) getOrElse false) flatMap (_ lift returnIndex))
  }
  
  def findOriginalTreeFromPosition(p: Position): Option[Tree] = {
    cuRoot(p) flatMap (_ find (_ samePos p))
  }

  def findOriginalTree(t: Tree) = findOriginalTreeFromPosition(t.pos)
  
}