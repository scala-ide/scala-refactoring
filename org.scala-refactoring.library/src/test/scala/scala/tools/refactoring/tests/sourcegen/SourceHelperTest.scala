/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.sourcegen

import tests.util.TestHelper
import org.junit.Assert._
import sourcegen.CommentsUtils
import tools.nsc.util.SourceFile
import tools.nsc.util.BatchSourceFile

class SourceHelperTest extends TestHelper {

  import CommentsUtils._

  @Test
  def liftSingleLineComment(): Unit = {

    assertEquals(("abc   ", "   //x"), splitComment("abc//x"))

    assertEquals(("x    x", " /**/ "), splitComment("x/**/x"))

    assertEquals(("5    *5", " /**/  "), splitComment("5/**/*5"))

    assertEquals(("5        *5", " /*/**/*/  "), splitComment("5/*/**/*/*5"))

    assertEquals(("4        /2", " /*/**/*/  "), splitComment("4/*/**/*//2"))
  }

  @Test
  def multiplication() = {
    assertEquals("""
    object A {
      val r = 3
      val p = r    * r
    }""", stripComment("""
    object A {
      val r = 3
      val p = r/**/* r
    }"""))
  }

  @Test
  def stripCommentInClass() = {
    assertEquals(stripWhitespacePreservers("""
    class A {
      def extractFrom(): Int = {
        val a = 1
        a + 1     ▒
      }
    }"""), stripComment("""
    class A {
      def extractFrom(): Int = {
        val a = 1
/*(*/   a + 1/*)*/
      }
    }"""))
  }
}

