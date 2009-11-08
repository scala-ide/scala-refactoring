package scala.tools.refactor.tests.utils

import org.junit.Assert._

import scala.tools.refactor.printer._

trait TestHelper {

  import Compiler._
  
  class TestString(src: String) {
    def partitionsInto(expected: String) = {
      val generatedCode = Partitioner(compiler, treeFrom(src)) map (_.print) mkString "|"
      assertEquals("|"+ expected+ "|", generatedCode)
    }
  }
  
  implicit def stringToTestString(src: String) = new TestString(src)
}
