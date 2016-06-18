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

  @Test
  def testWithOneSetCursorFromRightSelection(): Unit = {
    assertEquals(Range(5, 6), TextSelections.extractOne("012345/*<-cursor*/"))
    assertEquals(Range(5, 6), TextSelections.extractOne("012345/*<-cursor-0*/"))
    assertEquals(Range(4, 5), TextSelections.extractOne("012345/*<-cursor-1*/"))
    assertEquals(Range(3, 4), TextSelections.extractOne("012345/*<-cursor-2*/"))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testWithCursorInvalidCursorFromRightSelection(): Unit = {
    TextSelections.extractOne("0123/*<-cursor-18*/")
  }

  @Test
  def testWithOneSetCursorFromLeftSelection(): Unit = {
    assertEquals(Range(14, 15), TextSelections.extractOne("  /*cursor->*/45678"))
    assertEquals(Range(14, 15), TextSelections.extractOne("/*0-cursor->*/45678"))
    assertEquals(Range(15, 16), TextSelections.extractOne("/*1-cursor->*/45678"))
    assertEquals(Range(16, 17), TextSelections.extractOne("/*2-cursor->*/45678"))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testWithCursorInvalidCursorFromLeftSelection(): Unit = {
    TextSelections.extractOne("/*cursor->*/")
  }
}
