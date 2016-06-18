package scala.tools.refactoring.tests.util

import org.junit.Test
import org.junit.Assert.assertEquals
import TextSelections.Range


class TextSelectionsTest {
  @Test(expected = classOf[IllegalArgumentException])
  def testWithEmptyString(): Unit = {
    TextSelections.extractOne("")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testWithMoreThanOneSelection(): Unit = {
    TextSelections.extractOne("/*(*/ /*)*/ /*<-*/")
  }

  @Test
  def testWithOneEmptySelection(): Unit = {
    assertEquals(Range(0, 0), TextSelections.extractOne("/*<-*/"))
    assertEquals(Range(5, 5), TextSelections.extractOne("/*(*//*)*/"))
  }

  @Test
  def testWithOneNormalSelection(): Unit = {
    assertEquals(Range(5, 6), TextSelections.extractOne("/*(*/ /*)*/)"))
  }
}
