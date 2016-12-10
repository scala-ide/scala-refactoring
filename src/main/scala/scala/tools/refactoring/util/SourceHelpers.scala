package scala.tools.refactoring.util

import scala.annotation.tailrec
import scala.math.min

object SourceHelpers {
  def isRangeWithin(text: String, selection: SourceWithSelection): Boolean = {
    if (selection.length > text.length || selection.source.length < text.length) {
      false
    } else {
      val maxStepsBack = min(text.length - selection.length, selection.start)

      @tailrec
      def tryMatchText(stepsBack: Int = 0): Boolean = {
        if (stepsBack > maxStepsBack) {
          false
        } else {
          val start = selection.start - stepsBack

          @tailrec
          def matchSlice(i: Int = 0): Boolean = {
            if (i >= text.length) {
              true
            } else {
              if (text.charAt(i) != selection.source.charAt(start + i)) {
                false
              } else {
                matchSlice(i + 1)
              }
            }
          }

          if (start + text.length <= selection.source.length && matchSlice()) {
            true
          } else {
            tryMatchText(stepsBack + 1)
          }
        }
      }

      tryMatchText()
    }
  }
}
