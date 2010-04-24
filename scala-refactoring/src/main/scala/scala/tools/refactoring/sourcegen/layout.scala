package scala.tools.refactoring
package sourcegen

import tools.nsc.util.SourceFile

trait Layout {
  def asString: String
}

object Layout {
  def unapply(l: Layout) = Some(l.asString)
}

class LayoutFromFile(from: Int, to: Int, source: SourceFile) extends Layout {
  def asString = source.content.slice(from, to) mkString
}

class LayoutFromString(layout: String) extends Layout {
  def asString = layout
}