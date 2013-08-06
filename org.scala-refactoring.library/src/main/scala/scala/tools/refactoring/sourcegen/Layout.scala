/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import scala.reflect.internal.util.SourceFile

trait Layout {
  self =>

  def contains(s: String) = withoutComments.contains(s)

  def matches(r: String) = withoutComments.matches(r)

  /**
   * @return Returns this layout as a string but without comments.
   *         Comments are replaced by whitespace.
   */
  lazy val withoutComments = CommentsUtils.stripComment(asText)

  def asText: String

  override def toString() = asText

  def ++ (o: Layout) = o match {
    case NoLayout => this
    case _ => new Layout {
      override def asText = self.asText + o.asText
    }
  }

  def ++ (o: Fragment): Fragment = new Fragment {
    val leading  = o.pre(self, o.leading)
    val center   = o.center
    val trailing = o.trailing

    override val pre  = if (o.pre.isRequired(this.leading, NoLayout)) o.pre else NoRequisite
    override val post = o.post
  }

  def ++ (r: Requisite): Fragment = new EmptyFragment {
    override val trailing = self
    override val post = r
  }
}

case object NoLayout extends Layout {
  val asText = ""
}

object Layout {

  case class LayoutFromFile(source: SourceFile, start: Int, end: Int) extends Layout {

    lazy val asText = source.content.slice(start, end).mkString

    def splitAfter(cs: Char*): (Layout, Layout) = splitFromLeft(cs) match {
      case None => this → NoLayout
      case Some(i) => copy(end = i+1) → copy(start = i+1)
    }

    def splitAfterLast(cs: Char*): (Layout, Layout) = splitFromRight(cs) match {
      case None => this → NoLayout
      case Some(i) => copy(end = i+1) → copy(start = i+1)
    }

    def splitAtAndExclude(cs: Char*): (Layout, Layout) = splitFromLeft(cs) match {
      case None => this → NoLayout
      case Some(i) => copy(end = i) → copy(start = i+1)
    }

    def splitBefore(cs: Char*): (Layout, Layout) = splitFromLeft(cs) match {
      case None => NoLayout → this
      case Some(i) => copy(end = i) → copy(start = i)
    }

    private def splitFromLeft(cs: Seq[Char]): Option[Int] = split(cs, c => withoutComments.indexOf(c))

    private def splitFromRight(cs: Seq[Char]): Option[Int] = split(cs, c => withoutComments.lastIndexOf(c))

    private def split(cs: Seq[Char], findIndex: Char => Int): Option[Int] = cs.toList match {
      case Nil =>
        None
      case x :: xs =>
        val i = findIndex(x)
        if(i >= 0 ) {
          Some(start + i)
        } else
          split(xs, findIndex)
    }
  }

  def apply(source: SourceFile, start: Int, end: Int) = LayoutFromFile(source, start, end)

  def apply(s: String) = new Layout {
    val asText = s
  }
}
