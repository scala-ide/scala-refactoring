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
 * </ul>
 */
object TextSelections {
  final val StartMarker = "/*(*/"
  final val EndMarker = "/*)*/"
  final val EmptyMarker = "/*<-*/"

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

  private def extract(text: String): Seq[Range] = {
    extractStartToEndSelections(text) ++ extractEmptyMarkerSelections(text)
  }
}
