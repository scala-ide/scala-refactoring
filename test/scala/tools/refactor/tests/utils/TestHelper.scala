package scala.tools.refactor.tests.utils

import org.junit.Assert._

import scala.tools.refactor.printer._

trait TestHelper extends Partitioner {

  import Compiler._
  
  class TestString(src: String) extends Partitioner {
    def partitionsInto(expected: String) = {
      val generatedCode = splitIntoParts(compiler, treeFrom(src)) map (_.print) mkString "|"
      assertEquals("|"+ expected+ "|", generatedCode)
    }
  }
  
  implicit def stringToTestString(src: String) = new TestString(src)
}
