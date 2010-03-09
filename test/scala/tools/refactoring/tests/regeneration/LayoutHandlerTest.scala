package scala.tools.refactoring.tests.regeneration

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.regeneration._

@Test
class LayoutHandlerTest extends TestHelper {
  
  // parameters are: layout, existing indentation (scope, element), isEndOfScope, currentScopeIndentation
  def → = fixIndentation _
  
  @Test
  def testFixExistingIndentation() = {

    assertEquals("\n  A",       →("\n  A"  , Some(0, 2), false, 0))
    assertEquals("\n    A",     →("\n    A", Some(0, 4), false, 0))
    
    assertEquals("\n    A",     →("\n  A"  , Some(0, 2), false, 2))
    assertEquals("\n      A",   →("\n    A", Some(0, 4), false, 2))
  }
  
  @Test
  def testFixExistingIndentationMultipleLines() = {

    assertEquals("\n  \n  A",           →("\n  \n  A", Some(0, 2), false, 0))
    assertEquals("\n    \n    A",       →("\n  \n  A", Some(0, 2), false, 2))
    assertEquals("\n      \n      A",   →("\n  \n  A", Some(0, 2), false, 4))
  }

  @Test
  def testNewIndentation() = { 
    // should just indent for the scope + default indentation (2)
    assertEquals("\n  C",       →("\nC"  , None, false, 0))
    assertEquals("\n   C",      →("\nC"  , None, false, 1))
    assertEquals("\n    C",     →("\nC"  , None, false, 2))
    assertEquals("\n      C",   →("\nC"  , None, false, 4))
  }
    
  @Test
  def testNewIndentationEndOfScope() = { 
    // should just indent for the scope's indentation
    assertEquals("\nC",       →("\nC"  , None, true, 0))
    assertEquals("\n C",      →("\nC"  , None, true, 1))
    assertEquals("\n  C",     →("\nC"  , None, true, 2))
    assertEquals("\n    C",   →("\nC"  , None, true, 4))
  }
  
  @Test
  def testExtraSpaces() = { 
    // should just indent for the scope's indentation
    assertEquals("  \nC", →("  \nC"  , None, true, 0))
  }

  def req(r: String) = (new Fragment { requireAfter(new Requisite(r)); requireBefore(new Requisite(r)); val print = "": Seq[Char] })
  def req(r1: String, r2: String) = (new Fragment { requireAfter(Requisite(r1, r2)); requireBefore(Requisite(r1, r2)); val print = "": Seq[Char] })
  
  @Test
  def simpleRequisites() = {
    assertEquals("{}", processRequisites(req("{"), "", "", req("}")))
    assertEquals("{}", processRequisites(req("{"), "{", "", req("}")))
    assertEquals("{}", processRequisites(req("{"), "", "}", req("}")))
    assertEquals("{}", processRequisites(req("{"), "{", "}", req("}")))
    
    assertEquals("111{222}", processRequisites(req("{"), "111", "222", req("}")))
  } 
  
  @Test
  def checkWriteRequisites() = {
    assertEquals("x{xy}y", processRequisites(req("{", "x{x"), "", "", req("}", "y}y")))
    assertEquals("{y}y",   processRequisites(req("{", "x{x"), "{", "", req("}", "y}y")))
    assertEquals("x{x}",   processRequisites(req("{", "x{x"), "", "}", req("}", "y}y")))
    assertEquals("{}",     processRequisites(req("{", "x{x"), "{", "}", req("}", "y}y")))
  }
  
  @Test
  def requisitesWithComments() = {
    assertEquals("/*{*/{}", processRequisites(req("{"), "/*{*/", "", req("}")))
    assertEquals("{/*{*/}", processRequisites(req("{"), "", "/*{*/", req("}")))
  }
  
  @Test
  def insertsBeforeFirstNewline() = {
    assertEquals("aaa{\naaabbb\nbbb}", processRequisites(req("{"), "aaa\naaa", "bbb\nbbb", req("}")))
  }
  
  //@Test TODO
  def overlappingRequisites() = {
    
    def req(r: String) = (new Fragment { requireAfter(new Requisite(r)); requireBefore(new Requisite(r)); val print = "": Seq[Char] })
    
    assertEquals("123", processRequisites(req("23"), "12", "", req("")))
    
    assertEquals("123", processRequisites(req("123"), "1", "", req("")))
  }
  
  
  @Test
  def testClassParameters() = {
    "class A ( i: /*c*/Int, s: String)"     splitsInto "«»class ▒«A» (▒ «i»▒: /*c*/«Int»▒«s»▒: «String»▒)«»▒"
    "class A(i: Int, s: String, f: Float)"  splitsInto "«»class ▒«A»(▒«i»▒: «Int»▒«s»▒: «String»▒«f»▒: «Float»▒)«»▒"
    "class A(/*->*/i: Int/*<-*/)"           splitsInto "«»class ▒«A»(▒/*->*/«i»▒: «Int»/*<-*/▒)«»▒"
  }
  
  @Test
  def testImports() = {
    """
    package test {
      //test
      import _root_.scala.collection.mutable.ListBuffer
      import java.lang.String
  
      object Main
    }
    """ splitsInto 
    """«»
▒
    package «test» {
      //test
▒
      import «_root_».▒«scala».▒«collection».▒«mutable».▒«ListBuffer»
▒
      import «java».▒«lang».▒«String»
▒
  
      «»object ▒«Main»▒«»▒
▒
    }
    «»▒"""
  }
  
  
  @Test
  def testClassMembers() = {
    """
      class A {
        val a: Int = 5
      }
    """ splitsInto 
    """«»
▒
      «»class ▒«A» ▒«»{
▒
        «val» ▒«a»▒: «Int» = ▒«5»
▒
      }«»▒▒«»▒
▒
    «»▒"""
  }
  
  @Test
  def splitCommentWithComma(): Unit = {
    "class A ( i: /*,*/ Int )" splitsInto "«»class ▒«A» (▒ «i»▒: /*,*/ «Int» ▒)«»▒"
  }  
  
  @Test
  def splitCommentWithClosingParenthesis(): Unit = {
    "class A ( i: /*(*/ Int )" splitsInto "«»class ▒«A» (▒ «i»▒: /*(*/ «Int» ▒)«»▒"
  }  
  
  @Test
  def splitCommentWithOpeningParenthesis(): Unit = {
    "class A ( i: /*)*/ Int )" splitsInto "«»class ▒«A» (▒ «i»▒: /*)*/ «Int» ▒)«»▒"
  } 
  
  @Test
  def dontSplitEmptyParenthesis(): Unit = {
    "class A() extends AnyRef" splitsInto "«»class ▒«A»() ▒extends «AnyRef»▒«»▒"
  }
}

