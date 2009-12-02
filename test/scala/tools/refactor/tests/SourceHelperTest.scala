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
  def testForwards() = {
    assertEquals(Some(2), forwardsTo('{', 10)(0, " x{"))
    assertEquals(Some(0), forwardsTo('{', 10)(0, "{"))
    assertEquals(Some(5), forwardsTo('{', 10)(2, "{ x  {"))
    
    assertEquals(None, forwardsTo('{', 1)(0, " x  {"))
    assertEquals(None, forwardsTo('{', 10)(0, "  x  "))
    assertEquals(None, forwardsTo('{', 10)(10, " x  "))
  }
    
  @Test
  def testSkipWhitespace() = {
    assertEquals(Some(3), skipWhitespaceTo('{')(0, "  {"))
    assertEquals(Some(3), skipWhitespaceTo('{')(1, "  {"))
    assertEquals(Some(3), skipWhitespaceTo('{')(2, "  {"))
    assertEquals(None   , skipWhitespaceTo('{')(3, "  {"))
    
    assertEquals(Some(4), skipWhitespaceTo('{')(2, "{  {"))
    assertEquals(None, skipWhitespaceTo('{')(0, "xxx{"))
    assertEquals(None, skipWhitespaceTo('{')(0, "    "))
    assertEquals(None, skipWhitespaceTo('{')(10, "    "))
  }
  
  @Test
  def testBackwardsSkipWhitespace() = {
    assertEquals(Some(1), backwardsSkipWhitespaceTo('{')(4, " {  "))
    assertEquals(Some(1), backwardsSkipWhitespaceTo('{')(3, " {  "))
    assertEquals(Some(1), backwardsSkipWhitespaceTo('{')(2, " {  "))
    assertEquals(Some(1), backwardsSkipWhitespaceTo('{')(1, " {  "))
    assertEquals(None   , backwardsSkipWhitespaceTo('{')(0, " {  "))
    
    assertEquals(None   , backwardsSkipWhitespaceTo('{')(3, " {x "))
    assertEquals(None   , backwardsSkipWhitespaceTo('{')(3, " xx "))
    assertEquals(None   , backwardsSkipWhitespaceTo('{')(3, "    "))
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
"""
    
    assertEquals(2, indentationLength(5, src))
    assertEquals(4, indentationLength(17, src))
    assertEquals(4, indentationLength(30, src))
  }
}

