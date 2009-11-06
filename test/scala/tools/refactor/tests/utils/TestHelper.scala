package scala.tools.refactor.tests.utils

import org.junit.Assert._

import scala.tools.refactor.printer._

trait TestHelper {

  import Compiler._
  
  class TestString(src: String) {
    def partitionsInto(expected: String) = {
      val buffer = new java.lang.StringBuilder
      Partitioner(compiler, treeFrom(src)) foreach { part => part.print(buffer); buffer append "|"}
      assertEquals(expected, buffer.toString.substring(1).reverse.substring(2).reverse)
    }
  }
  
  implicit def stringToTestString(src: String) = new TestString(src)
}
