package scala.tools.refactor.tests.utils

import org.junit.Assert._

import scala.tools.refactor.printer._

trait TestHelper extends Partitioner with Merger {

  import Compiler._
  
  def parts(src: String) = splitIntoParts(compiler, treeFrom(src))
  
  class TestString(src: String) extends Partitioner {
    def partitionsInto(expected: String) = {
      val generatedCode = splitIntoParts(compiler, treeFrom(src)) map (_.print) mkString "|"
      assertEquals("|"+ expected+ "|", generatedCode)
    }
    
    def splitsInto(expected: String) = {
      def splitAllWhitespaces(parts: List[Part]): String = parts match {
        case x :: y :: xs =>
          val (ws, rest) = (y :: xs).span(_.isWhitespace)
          val (left, right) = splitWhitespaceBetween(x, ws, rest.head)
          
          x.print + left +"▒"+ right + splitAllWhitespaces(rest)
        case _ => ""
      }
      
      assertEquals(expected, splitAllWhitespaces(parts(src)))
    }
  }
  
  implicit def stringToTestString(src: String) = new TestString(src)
}
