package scala.tools.refactoring.util

import scala.annotation.tailrec
import scala.math.min

object SourceHelpers {

  /**
   * Decides whether a selection lies within a given text
   *
   * This is best explained by a few examples (selections are indicated by `[]`):
   * <ul>
   *  <li> `isRangeWithin("a", "[a]") == true`</li>
   *  <li> `isRangeWithin("ab", "[a]ce") == false` </li>
   *  <li> `isRangeWithin("ab", "[a]bc") == true` </li>
   *  <li> `isRangeWithin("ab", "a[b]c") == true` </li>
   *  <li> `isRangeWithin("ab", "ab[c]") == false` </li>
   * </ul>
   */
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
