package scala.tools.refactoring.tests.utils

import org.junit.Assert._

import scala.tools.refactoring._
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.transformation._
import scala.collection.mutable.ListBuffer

trait TestHelper extends Partitioner with Merger with CompilerProvider with Transform with LayoutHandler with TreePrinter with Tracing with Selections {
  
  def parts(src: String) = splitIntoFragments(treeFrom(src))
  
  def findMarkedNodes(src: String, tree: global.Tree) = {
    
    val start = src.indexOf("/*(*/")
    val end   = src.indexOf("/*)*/")
    
    new TreeSelection(tree, start, end)
  }
  
  class TestString(src: String) {
    
    def partitionsInto(expected: String) = {
      val p = splitIntoFragments(treeFrom(src))
      val generatedCode = p.toString
      assertEquals(expected, generatedCode)
    }
    
    def essentialFragmentsAre(expected: String) = {
      val tree = treeFrom(src)
      val generatedCode = essentialFragments(tree, new FragmentRepository(splitIntoFragments(tree))).toString
      assertEquals(expected, generatedCode)
    }
    
    def splitsInto(expected: String) = {
      def splitAllLayouts(parts: List[Fragment]): String = parts match {
        case x :: y :: xs =>
          val (ws, rest) = (y :: xs).span(_.isLayout)
          val (left, right) = splitLayoutBetween(Some(x, ws, rest.head))
          
          x.print + left +"▒"+ right + splitAllLayouts(rest)
        case _ => ""
      }
      
      //XXX assertEquals(expected, splitAllLayouts(parts(src)))
    }
    
    def transformsTo(expected: String, transform: global.Tree => global.Tree) {
      
      val tree = treeFrom(src)
      val newTree = transform(tree)
      
      val partitionedOriginal = splitIntoFragments(tree)
      val parts = new FragmentRepository(partitionedOriginal)
      val partitionedModified = essentialFragments(newTree, parts)
      
      val merged = merge(partitionedModified, parts)
          
      assertEquals(expected, merged map (_.print) mkString "")
    }
  }
  
  implicit def stringToTestString(src: String) = new TestString(src)
}
