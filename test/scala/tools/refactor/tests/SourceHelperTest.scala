package scala.tools.refactor.tests

import utils.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactor.printer._
import scala.tools.nsc.ast.Trees

@Test
class SourceHelperTest extends TestHelper {
  
  import SourceHelper._
  
  @Test
  def singleLineIndentation() = {
    assertEquals(0, indentationLength(1, "xx".toCharArray))
    assertEquals(1, indentationLength(3, " aaaa".toCharArray))
    assertEquals(4, indentationLength(6, "    aaaa".toCharArray))
  } 
  
  @Test
  def multiLineIndentation() = {
    
    val src = """
  class A
    def x
    // type T
""".toCharArray
    
    assertEquals(2, indentationLength(5, src))
    assertEquals(4, indentationLength(17, src))
    assertEquals(4, indentationLength(30, src))
  }
}

