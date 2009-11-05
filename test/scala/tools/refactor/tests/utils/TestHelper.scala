package scala.tools.refactor.tests.utils

import org.junit.Assert._

import scala.tools.refactor.printer._

trait TestHelper {

  import Compiler._
  
  class TestString(src: String) {
    def partitionsInto(expected: String) = assertEquals(expected, Partitioner(compiler, treeFrom(src)) mkString "|")
  }
  
  implicit def stringToTestString(src: String) = new TestString(src)
  
  def assert : this.type = this 
   
  def print(src: String) = {
    val buffer = new java.lang.StringBuilder
    Partitioner(compiler, treeFrom(src)) foreach (_.print(buffer))
    assertEquals(src, buffer.toString)
  }
}
