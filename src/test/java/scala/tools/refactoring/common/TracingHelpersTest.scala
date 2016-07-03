package scala.tools.refactoring.common

import org.junit.Test
import org.junit.Assert._


class TracingHelpersTest {
  import TracingHelpers._
  
  @Test
  def compactifyWithShortMsgs(): Unit = {
    assertEquals("", compactify(""))
    assertEquals("xxx", compactify("xxx"))
  }
  
  @Test
  def compactifyWithMultilineMsg(): Unit = {
    val mlMsg = "1. Do\n2. Make\n3. Say\n4. Think"
    assertEquals("1. Do...(3 more lines ommitted)", compactify(mlMsg))
  }
  
  @Test
  def compactifyWithVeryLongMsg(): Unit = {
    val looooongMsg = "Loooooooonger, then eeeeeeeeeeever before!!!!!!!!!!!!!!!!!"
    val compactified = compactify(looooongMsg)
    assertTrue(compactified.length < looooongMsg.length)
    assertTrue(compactified.startsWith("Loooooooonger"))
  }
}