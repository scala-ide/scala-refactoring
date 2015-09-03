/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.common

import tests.util.TestHelper
import org.junit.Assert._
import common.Selections

class SelectionsTest extends TestHelper {

  import global._

  private def getIndexedSelection(src: String) = {
    val tree = treeFrom(src)
    val start = commentSelectionStart(src)
    val end   = commentSelectionEnd(src)
    FileSelection(tree.pos.source.file, tree, start, end)
  }

  def selectedLocalVariable(expected: String, src: String) = {

    val selection = getIndexedSelection(src)

    assertEquals(expected, selection.selectedSymbolTree.get.symbol.name.toString)
  }

  def assertSelection(expectedTrees: String, expectedSymbols: String, src: String) = {

    val selection = getIndexedSelection(src)

    assertEquals(expectedTrees, selection.allSelectedTrees map (_.getClass.getSimpleName) mkString ", ")
    assertEquals(expectedSymbols, selection.selectedSymbols mkString ", ")
  }

  @Test
  def findValDefInMethod() = {
    assertSelection(
        "ValDef, Apply, Select, Ident, Ident",
        "value b, method +, value a, value i", """
      package findValDefInMethod
      class A {
        def addThree(i: Int) = {
          val a = 1
 /*(*/    val b = a + i  /*)*/
          val c = b + 1
          c
        }
      }
    """)
  }

  @Test
  def findIdentInMethod() = {
    assertSelection("Ident", "value i", """
      package findIdentInMethod
      class A {
        def addThree(i: Int) = {
          val a = 1
          val b = a +   /*(*/  i  /*)*/
          val c = b + 1
          c
        }
      }
    """)
  }

  @Test
  def findInMethodArguments() = {
    assertSelection("ValDef, TypeTree", "value i", """
      package findInMethodArguments
      class A {
        def addThree(/*(*/   i : Int   /*)*/) = {
          i
        }
      }
    """)
  }

  @Test
  def findWholeMethod() = {
    assertSelection(
        "DefDef, ValDef, TypeTree, Apply, Select, Ident, Literal",
        "method addThree, value i, method *, value i", """
      package findWholeMethod
      class A {
/*(*/
        def addThree(i: Int) = {
          i * 5
        }
/*)*/
      }
    """)

  }
  @Test
  def findNothing() = {
    assertSelection("", "", """
      package findNothing
      class A {
        /*(*/ /*)*/
        def addThree(i: Int) = {
          i * 5
        }
      }
    """)
  }

  @Test
  def findSelectedLocal() = {
    selectedLocalVariable("copy", """
      package findSelectedLocal
      class A {
        def times5(i: Int) = {
          val /*(*/copy/*)*/ = i
          copy * 5
        }
      }
    """)
  }

  @Test
  def selectedTheFirstCompleteSymbol() = {
    selectedLocalVariable("i", """
      package selectedTheFirstCompleteSymbol
      class A {
        def times5(i: Int) = {
          val /*(*/copy = i /*)*/
          copy * 5
        }
      }
    """)
  }

  @Test
  def selectedTheFirstSymbol() = {
    selectedLocalVariable("copy", """
      package selectedTheFirstSymbol
      class A {
        def times5(i: Int) = {
          /*(*/ val copy = i /*)*/
          copy * 5
        }
      }
    """)
  }
}

