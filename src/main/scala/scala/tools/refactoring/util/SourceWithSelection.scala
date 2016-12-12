package scala.tools.refactoring.util

case class SourceWithSelection(source: IndexedSeq[Char], start: Int, end: Int) {
  require(start > -1 && end >= start)
  require(end <= source.length)

  def length = end - start
}
