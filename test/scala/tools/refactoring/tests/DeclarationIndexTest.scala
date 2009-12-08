package scala.tools.refactoring.tests

import scala.tools.refactoring.tests.utils.TestHelper
import org.junit.{Test, Before}
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.Selections
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.analysis.{DeclarationIndexes, TreeAnalysis}
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.{SourceFile, BatchSourceFile, RangePosition}

@Test
class DeclarationIndexTest extends TestHelper with DeclarationIndexes with TreeAnalysis {

  import global._
  
  def withIndex(src: String)(body: (Tree, DeclarationIndex) => Unit ) {
    val index = new DeclarationIndex
    val tree = treeFrom(src)
    index.processTree(tree)
    body(tree, index)
  }
  
  def assertDeclarationOfSelection(expected: String, src: String) = withIndex(src) { (tree, index) =>
  
    def findDeclarationFromSelection(src: String) = findMarkedNodes(src, tree).trees.head match {
      case t: SymTree => 
        assertTrue(index.children(t.symbol.owner) exists (t.symbol ==))
        index.declaration(t.symbol)
      case t => throw new Exception("found: "+ t)
    }
    assertEquals(expected, findDeclarationFromSelection(src).toString)
  }
  
  
  def assertInboundLocalDependencies(expected: String, src: String) = withIndex(src) { (tree, index) =>

    val selection = findMarkedNodes(src, tree)
    val in = inboundLocalDependencies(index, selection, selection.symbols.head.owner)
    assertEquals(expected, in mkString ", ")
  }
  
  @Test
  def findValReference() = {
    assertDeclarationOfSelection("private[this] val x: Int = 1", """
      object A {
        private[this] val x = 1
        val y = /*(*/  x  /*)*/
      }
      """)
  }  
  
  @Test
  def findValReferenceFromMethod() = {
    assertDeclarationOfSelection("private[this] val x: Int = 1", """
      object A {
        private[this] val x = 1
        def go {
          val y = /*(*/  x  /*)*/
        }
      }
      """)
  }  
  
  @Test
  def findShadowed() = {
    assertDeclarationOfSelection("""val x: java.lang.String = "a"""", """
      object A {
        private[this] val x = 1
        def go  = {
          val x = "a"
          val y = /*(*/  x  /*)*/
          y
        }
      }
      """)

    assertDeclarationOfSelection("""private[this] val x: Int = 1""", """
      object A {
        private[this] val x = 1
        def go = {
          val x = "a"
         /*(*/  this.x  /*)*/
        }
      }
      """)
  }  
    
  @Test
  def findMethod() = {
    assertDeclarationOfSelection("""def x(): Int = 5""", """
      object A {
        def x() = 5
        def go  = {
          val y = /*(*/  x  /*)*/ ()
          y
        }
      }
      """)
  }
  
  @Test
  def findMethodFromOtherClass() = {
    assertDeclarationOfSelection("""<stable> <accessor> def x: Int = A.this.x""", """
      class A {
        val x = 5
      }
      object B {
        def go  = {
          val a = new A
          val y = /*(*/  a.x  /*)*/
          y
        }
      }
      """)
  }  
  
  @Test
  def findInboudLocalAndParameter() = {
    
    assertInboundLocalDependencies("value i, value a", """
      class A {
        def addThree(i: Int) = {
          val a = 1
 /*(*/    val b = a + 1 + i  /*)*/
          val c = b + 1
          c
        }
      }
    """)
  }  
  
  @Test
  def findParameterDependency() = {
    
    assertInboundLocalDependencies("value i", """
      class A {
        def addThree(i: Int) = {
          val a = 1
 /*(*/    val b = for(x <- 0 to i) yield x  /*)*/
          "done"
        }
      }
    """)
  }  
  
  @Test
  def findNoDependency() = {
    
    assertInboundLocalDependencies("", """
      class A {
        def addThree(i: Int) = {
          val a = 1
 /*(*/    val b = 2 * 21  /*)*/
          b
        }
      }
    """)
  }  
  
  @Test
  def findOnClassLevel() = {
    
    assertInboundLocalDependencies("value a", """
      class A {
        val a = 1
 /*(*/  val b = a + 1 /*)*/

        def addThree(i: Int) = {
          val a = 1
          val b = 2 * 21  
          b
        }
      }
    """)
  }
}

