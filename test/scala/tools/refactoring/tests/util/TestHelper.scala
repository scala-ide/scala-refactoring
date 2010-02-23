package scala.tools.refactoring.tests.util

import org.junit.Assert._

import scala.tools.refactoring._
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.util._
import scala.tools.refactoring.transformation._
import scala.collection.mutable.ListBuffer

trait TestHelper extends Regeneration with CompilerProvider with Transformation with LayoutPreferences with SilentTracing with Selections {
  
  def parts(src: String) = splitIntoFragments(treeFrom(src))
  
  def findMarkedNodes(src: String, tree: global.Tree) = {
    
    val start = src.indexOf("/*(*/")
    val end   = src.indexOf("/*)*/")
    
    new TreeSelection(tree, start, end)
  }
  
  class TestString(src: String) {
    
    def partitionsInto(expected: String) = {
      val tree = treeFrom(src)
      val f = splitIntoFragments(tree)
      assertEquals(expected, f.toString)
    }
    
    def essentialFragmentsAre(expected: String) = {
      val tree = treeFrom(src)
      val fragments = splitIntoFragments(tree)
      val essentials = essentialFragments(tree, new FragmentRepository(fragments))
      val generatedCode = essentials.toString
      assertEquals(expected, generatedCode)
    }
    
    def splitsInto(expected: String) = {
      
      val root = parts(src)
      
      val fs = new FragmentRepository(root)
      
      def withLayout(current: Fragment): String = {
        splitLayoutBetween(fs getNext current) match {
          case(l, r) => l +"▒"+ r
        }
      }
      
      def innerMerge(scope: Scope): List[String] = scope.children flatMap {
        case current: Scope => innerMerge(current) ::: withLayout(current) ::  Nil
        case current => "«"+ (current.print mkString) +"»" :: withLayout(current) :: Nil
      }
      
      assertEquals(expected, innerMerge(root) mkString)
    }
    
    def transformsTo(expected: String, transform: global.Tree => global.Tree) {
      
      val tree = treeFrom(src)
      val newTree = transform(tree)
      
      val partitionedOriginal = splitIntoFragments(tree)
      val parts = new FragmentRepository(partitionedOriginal)
      val partitionedModified = essentialFragments(newTree, parts)
      
      val merged = merge(partitionedModified, parts)
          
      assertEquals(expected, merged map (_.render(parts) mkString)  mkString)
    }
  }
  
  implicit def stringToTestString(src: String) = new TestString(src)
}
