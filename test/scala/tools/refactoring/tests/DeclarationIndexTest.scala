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
  
    val declarations = findMarkedNodes(src, tree).trees.head match {
      case t: RefTree => 
        assertTrue(index.children(t.symbol.owner) exists (t.symbol ==))
        index.declaration(t.symbol)
      case t => throw new Exception("found: "+ t)
    }
    assertEquals(expected, declarations.toString)
  }  
  
  def assertReferencesOfSelection(expected: String, src: String) = withIndex(src) { (tree, index) =>
  
    val references = findMarkedNodes(src, tree).trees.head match {
      case t: DefTree => 
        index.references(t.symbol) map ( ref => ref.toString +" ("+ ref.pos.start +", "+ ref.pos.end +")" )
      case t => throw new Exception("found: "+ t)
    }
    assertEquals(expected, references mkString ", ")
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
  }

  @Test
  def findShadowedWithThis() = {
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
  def findReferencesToLocal() = {
    assertReferencesOfSelection("a (86, 87), a (98, 99)", """
      class A {
        def go  = {
 /*(*/    val a = 5      /*)*/
          val y = a
          a
        }
      }
      """)
  }
  
  @Test
  def findReferencesToMethod() = {
    assertReferencesOfSelection("""A.this.go (96, 98)""", """
      class A {
 /*(*/       
        def go() = {
          5
        } /*)*/
        val g = go()
      } 

      """)
  }  
  
  @Test
  def findReferencesToClass() = {
    assertReferencesOfSelection("""A (48, 49), A (104, 105)""", """
 /*(*/  class A   /*)*/

      class B extends A

      class C(a: A) {
        def get(a: A): A = new A
      }
      """)
  }
}

