package scala.tools.refactor.printer

import scala.tools.nsc.util.Position
import scala.tools.nsc.ast.Trees

object WhiteSpaceBetweenTrees {
  
  def space(t: Trees#Tree) = WhitespacePart(t.pos.start, t.pos.point, t.pos.source)
  def space(t: Trees#Tree, l: List[Trees#Tree]): OffsetablePart = if (l.isEmpty) NullPart else space(t, l.head)
  def space(l1: List[Trees#Tree], l2: List[Trees#Tree]): OffsetablePart = if (l2.isEmpty || l1.isEmpty) NullPart else space(l1.last, l2.head)
  def space(t1: Trees#Tree, t2: Trees#Tree): OffsetablePart = space(t1.pos, t2.pos)
  def space(p: Position, t: Trees#Tree): OffsetablePart = space(p, t.pos)
  def space(t: Trees#Tree, p: Position): OffsetablePart = space(t.pos, p)
  
  def space(p1: Position, p2: Position): OffsetablePart = (p1, p2) match {
    case _ if p2 precedes p1 => NullPart
    case _ if p1.end < p2.start => WhitespacePart(p1.end, p2.start, p1.source)
    case _ if p1.start < p2.start => WhitespacePart(p1.start, p2.start, p1.source)
    case _ if p1 == p2 => WhitespacePart(p1.start, p1.point, p1.source)
    case _ if (p2 includes p1) &&  (p2.start < p1.start) => WhitespacePart(p1.end, p2.end, p1.source)
    case _ if p1.end == p2.end => WhitespacePart(p1.start, p2.start, p1.source)
    case _ if p1.start == p2.start => WhitespacePart(p1.end, p2.point, p1.source)
    case _ => WhitespacePart(p1.start, p2.start, p1.source)
  }
}