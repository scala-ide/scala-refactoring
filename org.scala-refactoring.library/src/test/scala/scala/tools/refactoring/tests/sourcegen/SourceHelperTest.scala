/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.sourcegen

import tests.util.TestHelper
import org.junit.Assert._
import sourcegen.SourceCodeHelpers
import tools.nsc.util.BatchSourceFile

class SourceHelperTest extends TestHelper with SourceCodeHelpers {
  
  implicit def stringToSourceFile(s: String) = new BatchSourceFile(s, s)

  @Test
  def liftSingleLineComment(): Unit = {
      
    assertEquals(("abc   ", "   //x"), splitComment("abc//x"))
    
    assertEquals(("x    x", " /**/ "), splitComment("x/**/x"))
    
    assertEquals(("5    *5", " /**/  "), splitComment("5/**/*5"))
    
    assertEquals(("5        *5", " /*/**/*/  "), splitComment("5/*/**/*/*5"))
    
    assertEquals(("4        /2", " /*/**/*/  "), splitComment("4/*/**/*//2"))
  }
  
  @Test
  def liftMultiLineComment(): Unit = {
       
    liftComment("a/* \n//\n*/b") { s =>
      assertEquals("a   \n  \n  b", s)
      s
    }
    
    liftComment("\n abcd \n ab 5/4*3") { s =>
      assertEquals("\n abcd \n ab 5/4*3", s)
      s
    }
  }
   
  @Test
  def liftAndReplace() = {
    assertEquals("xxx//x", liftComment("abc//x")(_ => "xxx   "))

    assertEquals("xxx//x\na", liftComment("abc//x\na")(_ => "xxx   \na"))
    
    assertEquals("d/* \n//\n*/e", liftComment("a/* \n//\n*/b")(_ => "d   \n  \n  e"))
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
    assertEquals("""
    class A {
      def extractFrom(): Int = {
        val a = 1
        a + 1         
      }
    }""", stripComment("""
    class A {
      def extractFrom(): Int = {
        val a = 1
/*(*/   a + 1    /*)*/
      }
    }"""))
  }
}

