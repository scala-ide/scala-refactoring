package scala.tools.refactoring.tests.util

import scala.collection.immutable.Queue
import scala.annotation.tailrec

/**
 * Helper class to extract selections from unit test input.
 *
 * The following markers are supported:
 *
 * <ul>
 *   <li> `/*(*/` - start of the selection
 *   <li> `/*)*/` - end of the selection
 *   <li> `/*<-*/` - an empty selection
 *   <li> `/*cursor->*/` - mimics a cursor selection right after the comment
 *   <li> `/*n-cursor->*/*` - mimics a cursor selection `n` characters after the comment
 *   <li> `/*<-cursor*/` - mimics a cursor selection right before the comment
 *   <li> `/*<-cursor-n*/` - mimics a cursor selection `n` characters before the comment
 * </ul>
 */
object TextSelections {
  final val StartMarker = "/*(*/"
  final val EndMarker = "/*)*/"
  final val EmptyMarker = "/*<-*/"

  final val CursorFromLeft = """/\*(?:(\d+)-)?cursor->\*/""".r
  final val CursorFromRight = """/\*<-cursor(?:-(\d+))?\*/""".r

  final case class Range(from: Int, to: Int) {
    require(from <= to)
    require(from >= 0)
  }

  /**
   * Extracts a single selection from the given text
   *
   * @throws IllegalArgumentException unless exactly one selection is found
   */
  def extractOne(text: String): Range = {
    extract(text) match {
      case Seq(range) => range

      case other =>
        throw new IllegalArgumentException(
            s"Expected exactly one selection, but got $other\n" +
            s"  text:\n" +
            text.lines.map("    " + _).mkString("\n"))
    }
  }

  private def extractStartToEndSelections(text: String): Seq[Range] = {
    @tailrec
    def go(acc: Queue[Range] = Queue()): Queue[Range] = {
      val offset = acc.lastOption.map(_.to + EndMarker.length).getOrElse(0)

      text.indexOf(StartMarker, offset) match {
        case -1 => acc
        case startMarkerPos =>
          val from = startMarkerPos + StartMarker.length

          text.indexOf(EndMarker, offset) match {
            case -1 => acc
            case to =>
              go(acc :+ Range(from, to))
          }
      }
    }

    go()
  }

  private def extractEmptyMarkerSelections(text: String): Seq[Range] = {
    @tailrec
    def go(acc: Queue[Range] = Queue()): Queue[Range] = {
      val offset = acc.lastOption.map(_.to + 1).getOrElse(0)
      val pos = text.indexOf(EmptyMarker, offset)

      if (pos < 0) acc
      else go(acc :+ Range(pos, pos))
    }

    go()
  }

  private def extractCursorFromLeftSelections(text: String): Seq[Range] = {
    CursorFromLeft.findAllMatchIn(text).map { m =>
      val offset = m.end
      val shift = Option(m.group(1)).map(_.toInt).getOrElse(0)
      val from = offset + shift
      val to = from + 1

      if (to >= text.size) {
        throw selectionOutOfRange(text)
      } else {
        Range(from, to)
      }
    }.toSeq
  }

  private def extractCursorFromRightSelections(text: String): Seq[Range] = {
    CursorFromRight.findAllMatchIn(text).map { m =>
      val offset = m.start
      val shift = Option(m.group(1)).map(_.toInt).getOrElse(0)
      val from = offset - shift - 1
      val to = from + 1

      if (from < 0) {
        throw selectionOutOfRange(text)
      } else {
        Range(from, to)
      }
    }.toSeq
  }

  private def selectionOutOfRange(text: String) = {
    new IllegalArgumentException(
        "Selection out of range\n" +
        text.lines.map("  " + _).mkString("\n"))
  }

  private def extract(text: String): Seq[Range] = {
    extractStartToEndSelections(text) ++
      extractEmptyMarkerSelections(text) ++
      extractCursorFromLeftSelections(text) ++
      extractCursorFromRightSelections(text)
  }
}
