package scala.tools.refactor.tests

import utils.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactor.printer._

@Test
class WhitespaceSplitterTest extends TestHelper {
  
  @Test
  def simpleRequisites() = {
    
    def req(r: String) = (new Fragment { requireAfter(new Requisite(r)); requireBefore(new Requisite(r)); val print = "" })
    
    assertEquals("{}", processRequisites(req("{"), "", "", req("}")))
    assertEquals("{}", processRequisites(req("{"), "{", "", req("}")))
    assertEquals("{}", processRequisites(req("{"), "", "}", req("}")))
    assertEquals("{}", processRequisites(req("{"), "{", "}", req("}")))
    
    assertEquals("111{222}", processRequisites(req("{"), "111", "222", req("}")))
  } 
  
  @Test
  def checkWriteRequisites() = {
    
    def req(r1: String, r2: String) = (new Fragment { requireAfter(Requisite(r1, r2)); requireBefore(Requisite(r1, r2)); val print = "" })
    
    assertEquals("x{xy}y", processRequisites(req("{", "x{x"), "", "", req("}", "y}y")))
    assertEquals("{y}y",   processRequisites(req("{", "x{x"), "{", "", req("}", "y}y")))
    assertEquals("x{x}",   processRequisites(req("{", "x{x"), "", "}", req("}", "y}y")))
    assertEquals("{}",     processRequisites(req("{", "x{x"), "{", "}", req("}", "y}y")))
  }
  
  //@Test
  def overlappingRequisites() = {
    
    def req(r: String) = (new Fragment { requireAfter(new Requisite(r)); requireBefore(new Requisite(r)); val print = "" })
    
    assertEquals("123", processRequisites(req("23"), "12", "", req("")))
    
    assertEquals("123", processRequisites(req("123"), "1", "", req("")))
  }
  
  
  @Test
  def testClassParameters() = {
    "class A ( i: /*c*/Int, s: String)"     splitsInto "class ▒A (▒ i: /*c*/▒Int▒s: ▒String▒)"
    "class A(i: Int, s: String, f: Float)"  splitsInto "class ▒A(▒i: ▒Int▒s: ▒String▒f: ▒Float▒)"
    "class A(/*->*/i: Int/*<-*/)"           splitsInto "class ▒A(▒/*->*/i: ▒Int/*<-*/▒)"
  }
  
  @Test
  def testClassMembers() = {
    """
      class A {
        val a: Int
        val b: Int
        val c: Int
      }
    """ splitsInto 
    """
▒      class A {
▒        val a: ▒Int
▒        val b: ▒Int
▒        val c: ▒Int
▒      }
    """
  }
}

