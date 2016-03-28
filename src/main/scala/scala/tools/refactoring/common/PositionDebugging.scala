package scala.tools.refactoring.common

import scala.reflect.api.Position
import scala.reflect.internal.util.NoPosition
import scala.reflect.internal.util.NoSourceFile
import scala.tools.refactoring.getSimpleClassName

/**
 * Some utilities for debugging purposes.
 */
object PositionDebugging {
  def format(pos: Position): String = {
    formatInternal(pos, false)
  }

  def formatCompact(pos: Position): String = {
    formatInternal(pos, true)
  }

  def format(start: Int, end: Int, source: Array[Char]): String = {
    def slice(start: Int, end: Int): String = {
      source.view(start, end).mkString("").replace("\r\n", "\\r\\n").replace("\n", "\\n")
    }

    val ctxChars = 10
    val l = slice(start - ctxChars, start)
    val m = slice(start, end)
    val r = slice(end, end + ctxChars)
    s"$l«$m»$r".trim
  }

  private def formatInternal(pos: Position, compact: Boolean): String = {
    if (pos != NoPosition && pos.source != NoSourceFile) {
      val posType = getSimpleClassName(pos)

      val (start, point, end) = {
        if (!pos.isRange) (pos.point, pos.point, pos.point)
        else (pos.start, pos.point, pos.end)
      }

      val markerString = {
        if (start == end) s"($start)"
        else s"($start, $point, $end)"
      }

      val relevantSource = {
        if (compact) ""
        else "[" + format(start, end, pos.source.content) + "]"
      }

      s"$posType$markerString$relevantSource"
    } else {
      "UndefinedPosition"
    }
  }
}
