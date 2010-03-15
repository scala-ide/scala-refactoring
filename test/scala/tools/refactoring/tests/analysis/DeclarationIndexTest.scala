package scala.tools.refactoring.tests.analysis

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.{Test, Before}
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.analysis.{Indexes, TreeAnalysis}
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.{SourceFile, BatchSourceFile, RangePosition}

@Test
class DeclarationIndexTest extends TestHelper with Indexes with TreeAnalysis {

  import global._
  
  def withIndex(src: String)(body: Tree => Unit ) {
    val tree = treeFrom(src)
    index.processTree(tree)
    body(tree)
  }
  
  def assertDeclarationOfSelection(expected: String, src: String) = withIndex(src) { tree =>
  
    val declarations = findMarkedNodes(src, tree).trees.head match {
      case t: RefTree => 
        assertTrue("Symbol "+ t.symbol.owner +" does not have a child "+ t.symbol, index.children(t.symbol.owner) exists (_.symbol == t.symbol))
        index.declaration(t.symbol)
      case t => throw new Exception("found: "+ t)
    }
    assertEquals(expected, declarations.toString)
  }  
  
  def assertReferencesOfSelection(expected: String, src: String) = withIndex(src) { tree =>
  
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
    assertDeclarationOfSelection("""<stable> <accessor> def x: Int = N.this.x""", """
      class N {
        val x = 5
      }
      object M {
        def go  = {
          val a = new N
          val y = /*(*/  a.x  /*)*/
          y
        }
      }
      """)
  }  
  
  @Test
  def findReferencesToLocal() = {
    assertReferencesOfSelection("a (86, 87), a (98, 99)", """
      class H {
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
    assertReferencesOfSelection("""G.this.go (96, 98)""", """
      class G {
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
    assertReferencesOfSelection("""Z (71, 72), Z (127, 128)""", """
      package xyz
    
 /*(*/  class Z   /*)*/

      class B extends Z

      class C(a: Z) {
        def get(a: Z): Z = new Z
      }
      """)
  }
}

