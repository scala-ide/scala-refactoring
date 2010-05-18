package scala.tools.refactoring
package sourcegen

import tools.nsc.util.RangePosition
import tools.nsc.util.SourceFile

trait Layout {
  self =>
  
  def asText: String
  
  override def toString() = asText
  
  def ++ (other: Layout) = new Layout {
    def asText = self.asText + other.asText
  }
}

case class PrintingResult(leading: Layout, center: Layout, trailing: Layout) extends Layout {
  def asText = leading.asText + center.asText + trailing.asText
}

case class LayoutFromFile(source: SourceFile, start: Int, end: Int) extends Layout {

  lazy val asText = source.content.slice(start, end) mkString
        
  def splitAfter(cs: Char*): (Layout, Layout) = split(cs) match {
    case None => this → NoLayout
    case Some(i) => copy(end = i+1) → copy(start = i+1)
  }
  
  def splitBefore(cs: Char*): (Layout, Layout) = split(cs) match {
    case None => NoLayout → this
    case Some(i) => copy(end = i) →  copy(start = i)
  }
  
  private def split(cs: Seq[Char]): Option[Int] = cs.toList match {
    case Nil => 
      None
    case x :: xs if toString.indexOf(x) >= 0 =>
      Some(start + toString.indexOf(x))
    case _ :: xs => split(xs)
  }
}

case class LayoutFromString(val asText: String) extends Layout

case object NoLayout extends Layout {
  val asText = ""
} 
