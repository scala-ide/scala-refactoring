package scala.tools.refactoring.common

import scala.reflect.api.Position
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.NoSourceFile

/**
 * Some utilities for debugging purposes.
 */
object PositionDebugging {
  def format(pos: Position): String = {
    if (pos.source != NoSourceFile) {
      (pos.getClass.getSimpleName + "[" + format(pos.start, pos.end, pos.source.content) + "]")
    } else {
      "UndefinedPosition"
    }
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
}
