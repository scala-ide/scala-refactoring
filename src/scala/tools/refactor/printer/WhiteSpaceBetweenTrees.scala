package scala.tools.refactor.printer

import scala.tools.nsc.util.Position
import scala.tools.nsc.ast.Trees

object WhiteSpaceBetweenTrees {
  
  def space(t: Trees#Tree) = new WhiteSpacePart(t.pos.start, t.pos.point, t.pos.source)
  def space(t: Trees#Tree, l: List[Trees#Tree]): WhiteSpacePart = if (l.isEmpty) nullPart else space(t, l.head)
  def space(l1: List[Trees#Tree], l2: List[Trees#Tree]): WhiteSpacePart = if (l2.isEmpty || l1.isEmpty) nullPart else space(l1.last, l2.head)
  def space(t1: Trees#Tree, t2: Trees#Tree): WhiteSpacePart = space(t1.pos, t2.pos)
  def space(p: Position, t: Trees#Tree): WhiteSpacePart = space(p, t.pos)
  def space(t: Trees#Tree, p: Position): WhiteSpacePart = space(t.pos, p)
  
  def space(p1: Position, p2: Position) = (p1, p2) match {
    case _ if p2 precedes p1 => nullPart
    case _ if p1.end < p2.start => new WhiteSpacePart(p1.end, p2.start, p1.source)
    case _ if p1.start < p2.start => new WhiteSpacePart(p1.start, p2.start, p1.source)
    case _ if p1 == p2 => new WhiteSpacePart(p1.start, p1.point, p1.source)
    case _ if (p2 includes p1) &&  (p2.start < p1.start) => new WhiteSpacePart(p1.end, p2.end, p1.source)
    case _ if p1.end == p2.end => new WhiteSpacePart(p1.start, p2.start, p1.source)
    case _ if p1.start == p2.start => new WhiteSpacePart(p1.end, p2.point, p1.source)
    case _ => new WhiteSpacePart(p1.start, p2.start, p1.source)
  }
}