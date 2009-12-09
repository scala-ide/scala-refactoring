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
class TreeAnalysisTest extends TestHelper with DeclarationIndexes with TreeAnalysis {

  import global._
  
  def withIndex(src: String)(body: (Tree, DeclarationIndex) => Unit ) {
    val index = new DeclarationIndex
    val tree = treeFrom(src)
    index.processTree(tree)
    body(tree, index)
  }
  
  def assertInboundLocalDependencies(expected: String, src: String) = withIndex(src) { (tree, index) =>

    val selection = findMarkedNodes(src, tree)
    val in = inboundLocalDependencies(index, selection, selection.symbols.head.owner)
    assertEquals(expected, in mkString ", ")
  }
  
  def assertOutboundLocalDependencies(expected: String, src: String) = withIndex(src) { (tree, index) =>

    val selection = findMarkedNodes(src, tree)
    val out = outboundLocalDependencies(index, selection, selection.symbols.head.owner)
    assertEquals(expected, out mkString ", ")
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
  
  @Test
  def findDependencyOnMethod() = {
    
    assertInboundLocalDependencies("value i, method inc", """
      class A {
        def addThree(i: Int) = {
          def inc(j: Int) = j + 1
 /*(*/    val b = inc(inc(inc(i)))  /*)*/
          b
        }
      }
    """)
  }
   
  @Test
  def findOutboundDeclarations() = {
    
    assertOutboundLocalDependencies("b, b, b", """
      class A {
        def addThree = {
 /*(*/    val b = 1  /*)*/
          b + b + b
        }
      }
    """)
  }
     
  @Test
  def multipleReturnValues() = {
    
    assertOutboundLocalDependencies("a, b, c", """
      class A {
        def addThree = {
 /*(*/    val a = 'a'
          val b = 'b'
          val c = 'c'/*)*/
          a + b + c
        }
      }
    """)
  }
  
  @Test
  def dontReturnArgument() = {
    
    assertOutboundLocalDependencies("", """
      class A {
        def go = {
          var a = 1
 /*(*/    a = 2  /*)*/
          a
        }
      }
    """)
  }
  
}

