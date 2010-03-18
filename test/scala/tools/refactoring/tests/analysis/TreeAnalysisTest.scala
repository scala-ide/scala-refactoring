package scala.tools.refactoring.tests.analysis

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.{Test, Before}
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.analysis.{Indexes, TreeAnalysis}
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.{SourceFile, BatchSourceFile, RangePosition}

@Test
class TreeAnalysisTest extends TestHelper with Indexes with TreeAnalysis {

  import global._
  
  def withIndex(src: String)(body: (Index, Tree) => Unit ) {
    val tree = treeFrom(src)
    body(new Index {
      processTree(tree)
    }, tree)
  }
  
  def assertInboundLocalDependencies(expected: String, src: String) = withIndex(src) { (index, tree) =>

    val selection = findMarkedNodes(src, tree) get
    val in = inboundLocalDependencies(selection, selection.selectedSymbols.head.owner, index)
    assertEquals(expected, in mkString ", ")
  }
  
  def assertOutboundLocalDependencies(expected: String, src: String) = withIndex(src) { (index, tree) =>

    val selection = findMarkedNodes(src, tree) get
    val out = outboundLocalDependencies(selection, selection.selectedSymbols.head.owner, index)
    assertEquals(expected, out mkString ", ")
  }
  
  @Test
  def findInboudLocalAndParameter() = {
    
    assertInboundLocalDependencies("value i, value a", """
      class A9 {
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
      class A8 {
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
      class A7 {
        def addThree(i: Int) = {
          val a = 1
 /*(*/    val b = 2 * 21  /*)*/
          b
        }
      }
    """)
  }
  
  @Test
  def findDependencyOnMethod() = {
    
    assertInboundLocalDependencies("value i, method inc", """
      class A6 {
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
    
    assertOutboundLocalDependencies("value b, value b, value b", """
      class A5 {
        def addThree = {
 /*(*/    val b = 1  /*)*/
          b + b + b
        }
      }
    """)
  }
     
  @Test
  def multipleReturnValues() = {
    
    assertOutboundLocalDependencies("value a, value b, value c", """
      class TreeAnalysisTest {
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
      class TreeAnalysisTest {
        def go = {
          var a = 1
 /*(*/    a = 2  /*)*/
          a
        }
      }
    """)
  }
    
  @Test
  def findOnClassLevel() = {
    
    assertInboundLocalDependencies("value a", """
    class Outer {
      class B2 {
        val a = 1
 /*(*/  val b = a + 1 /*)*/

        def addThree(i: Int) = {
          val a = 1
          val b = 2 * 21  
          b
        }
      }
    }
    """)
  }
}

