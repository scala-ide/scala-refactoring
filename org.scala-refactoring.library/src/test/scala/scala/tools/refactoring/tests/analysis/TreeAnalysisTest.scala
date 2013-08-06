/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import tests.util.TestHelper
import org.junit.Assert._
import common.Selections
import analysis.GlobalIndexes
import analysis.TreeAnalysis

class TreeAnalysisTest extends TestHelper with GlobalIndexes with TreeAnalysis {

  import global._

  var index: IndexLookup = null

  def withIndex(src: String)(body: Tree => Unit ) {
    val tree = treeFrom(src)
    global.ask { () =>
      index = GlobalIndex(List(CompilationUnitIndex(tree)))
    }
    body(tree)
  }

  def findMarkedNodes(src: String, tree: Tree) = {
    val start = commentSelectionStart(src)
    val end   = commentSelectionEnd(src)
    FileSelection(tree.pos.source.file, tree, start, end)
  }

  def assertInboundLocalDependencies(expected: String, src: String) = withIndex(src) { tree =>

    val selection = findMarkedNodes(src, tree)
    val in = global.ask(() => inboundLocalDependencies(selection, selection.selectedSymbols.head.owner))
    assertEquals(expected, in mkString ", ")
  }

  def assertOutboundLocalDependencies(expected: String, src: String) = withIndex(src) { tree =>

    val selection = findMarkedNodes(src, tree)
    val out = global.ask(() => outboundLocalDependencies(selection, selection.selectedSymbols.head.owner))
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

    assertOutboundLocalDependencies("value b", """
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

    assertInboundLocalDependencies("", """
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

