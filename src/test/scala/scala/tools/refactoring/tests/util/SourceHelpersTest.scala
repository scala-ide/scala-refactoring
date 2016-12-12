package scala.tools.refactoring.tests.util

import org.junit.Test
import scala.tools.refactoring.util.SourceHelpers
import org.junit.Assert._
import scala.tools.refactoring.util.SourceWithSelection

class SourceHelpersTest {
  @Test
  def isRangeWithinWithTrivialArgs(): Unit = {
    testIsRangeWithin("", SourceWithSelection("x", 0, 1), false)
    testIsRangeWithin("x", SourceWithSelection("x", 0, 1), true)
    testIsRangeWithin("xx", SourceWithSelection("x", 0, 1), false)
  }

  @Test
  def isRangeWithEmptySelections(): Unit = {
    testIsRangeWithin("", SourceWithSelection("", 0, 0), true)
    testIsRangeWithin("a", SourceWithSelection("a", 0, 0), true)
    testIsRangeWithin("b", SourceWithSelection("a", 0, 0), false)
  }

  @Test
  def isRangeWithinWithSimpleExamples(): Unit = {
    testIsRangeWithin("abab", SourceWithSelection("0abab5", 4, 5), true)
    testIsRangeWithin("abab", SourceWithSelection("0abab5", 1, 5), true)
    testIsRangeWithin("abab", SourceWithSelection("0abab5", 1, 6), false)
    testIsRangeWithin("abab", SourceWithSelection("0abab5", 2, 4), true)
    testIsRangeWithin("ab", SourceWithSelection("012a", 3, 4), false)
    testIsRangeWithin("abba", SourceWithSelection("012abba", 3, 4), true)
  }

  private def testIsRangeWithin(text: String, selection: SourceWithSelection, expected: Boolean): Unit = {
    val result = SourceHelpers.isRangeWithin(text, selection)
    assertEquals(expected, result)
  }
}
