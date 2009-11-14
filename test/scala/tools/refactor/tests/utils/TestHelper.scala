package scala.tools.refactor.tests.utils

import org.junit.Assert._

import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

trait TestHelper extends Partitioner with Merger with CompilerProvider with Transform {
  
  def parts(src: String) = splitIntoParts(treeFrom(src))
  
  class TestString(src: String) {
    
    def partitionsInto(expected: String) = {
      val p = splitIntoParts(treeFrom(src))
      val generatedCode = p.toString
      assertEquals("❨|"+ expected +"|❩", generatedCode)
    }
    
    def essentialPartsAre(expected: String) = {
      val generatedCode = essentialParts(treeFrom(src)).toString
      assertEquals(expected, generatedCode)
    }
    
    def splitsInto(expected: String) = {
      def splitAllWhitespaces(parts: List[Part]): String = parts match {
        case x :: y :: xs =>
          val (ws, rest) = (y :: xs).span(_.isWhitespace)
          val (left, right) = splitWhitespaceBetween(x, ws, rest.head)
          
          x.print + left +"▒"+ right + splitAllWhitespaces(rest)
        case _ => ""
      }
      
      //assertEquals(expected, splitAllWhitespaces(parts(src)))
    }
    
    def transformsTo(expected: String, transform: compiler.Tree => compiler.Tree) {
      
      val tree = treeFrom(src)
      val newTree = transform(tree)
      
      val partitionedOriginal = splitIntoParts(tree)
      val partitionedModified = essentialParts(newTree)
      
      val merged = merge(partitionedModified, partitionedOriginal)
          
      assertEquals(expected, merged map (_.print) mkString "")
    }
  }
  
  implicit def stringToTestString(src: String) = new TestString(src)
}
