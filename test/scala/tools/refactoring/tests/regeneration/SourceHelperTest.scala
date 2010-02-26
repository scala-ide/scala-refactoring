package scala.tools.refactoring.tests.regeneration

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.regeneration._
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.BatchSourceFile

@Test
class SourceHelperTest extends TestHelper {
  
  implicit def stringToSourceFile(s: String) = new BatchSourceFile(s, s)
  
  @Test
  def testForwards() = {
    assertEquals(Some(2), forwardsTo('{', 10)(0, " x{"))
    assertEquals(Some(0), forwardsTo('{', 10)(0, "{"))
    assertEquals(Some(5), forwardsTo('{', 10)(2, "{ x  {"))
    
    assertEquals(None, forwardsTo('{', 1)(0, " x  {"))
    assertEquals(None, forwardsTo('{', 10)(0, "  x  "))
    assertEquals(None, forwardsTo('{', 10)(10, " x  "))
  }
  
  @Test
  def testSkipWithComments() = {
    assertEquals(Some(10), forwardsTo('{', 10)(0, " x  /*{*/ {"))
    assertEquals(Some(11), skipLayoutTo('{')(0, "   /*{*/  {"))
  }
    
  @Test
  def testSkipLayout() = {
    assertEquals(Some(3), skipLayoutTo('{')(0, "  {"))
    assertEquals(Some(3), skipLayoutTo('{')(1, "  {"))
    assertEquals(Some(3), skipLayoutTo('{')(2, "  {"))
    assertEquals(None   , skipLayoutTo('{')(3, "  {"))
    
    assertEquals(Some(4), skipLayoutTo('{')(2, "{  {"))
    assertEquals(None, skipLayoutTo('{')(0, "xxx{"))
    assertEquals(None, skipLayoutTo('{')(0, "    "))
    assertEquals(None, skipLayoutTo('{')(10, "    "))
  }
  
  @Test
  def testBackwardsSkipLayout() = {
    assertEquals(Some(1), backwardsSkipLayoutTo('{')(4, " {  "))
    assertEquals(Some(1), backwardsSkipLayoutTo('{')(3, " {  "))
    assertEquals(Some(1), backwardsSkipLayoutTo('{')(2, " {  "))
    assertEquals(Some(1), backwardsSkipLayoutTo('{')(1, " {  "))
    assertEquals(None   , backwardsSkipLayoutTo('{')(0, " {  "))
    
    assertEquals(None   , backwardsSkipLayoutTo('{')(3, " {x "))
    assertEquals(None   , backwardsSkipLayoutTo('{')(3, " xx "))
    assertEquals(None   , backwardsSkipLayoutTo('{')(3, "    "))
  }
  
  @Test
  def singleLineIndentation() = {
    assertEquals(0, indentationLength(1, "xx"))
    assertEquals(1, indentationLength(3, " aaaa"))
    assertEquals(4, indentationLength(6, "    aaaa"))
  } 
  
  @Test
  def multiLineIndentation() = {
    
    val src = """
  class A
    def x
    // type T
    def y
"""
    
    assertEquals(2, indentationLength(5, src))
    assertEquals(4, indentationLength(17, src))
    assertEquals(12, indentationLength(33, src))
  }
  
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

