package scala.tools.refactoring.tests

import scala.tools.refactoring.tests.util.{TestHelper, TreePath}
import org.junit.{Test, Before}
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.util.Selections
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.analysis.Indexes
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.{SourceFile, BatchSourceFile, RangePosition}

@Test
class SelectionsTest extends TestHelper with Indexes with TreePath {

  import global._
  
  private def getIndexedSelection(src: String) = {
    val tree = treeFrom(src)
    
    index.processTree(tree)
    
    findMarkedNodes(src, tree)
  }
  
  def selectedLocalVariable(expected: String, src: String) = {
    
    val selection = getIndexedSelection(src)
    
    assertEquals(expected, selection.selectedSymbolTree.get.symbol.name.toString)
  }
  
  def assertSelection(expectedTrees: String, expectedSymbols: String, src: String) = {
    
    val selection = getIndexedSelection(src)
    
    assertEquals(expectedTrees, selection.treesWithSubtrees map (_.getClass.getSimpleName) mkString ", ")
    assertEquals(expectedSymbols, selection.symbols mkString ", ")
  }
  
  @Test
  def findValDefInMethod() = {
    assertSelection(
        "ValDef, Apply, Select, Ident, Ident", 
        "value b, method +, value a, value i", """
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
      class A {
        def times5(i: Int) = {
          /*(*/ val copy = i /*)*/
          copy * 5
        }
      }
    """)
  }
}

