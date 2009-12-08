package scala.tools.refactoring.tests

import scala.tools.refactoring.tests.utils.{TestHelper, TreePath}
import org.junit.{Test, Before}
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.Selections
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.analysis.DeclarationIndexes
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.{SourceFile, BatchSourceFile, RangePosition}

@Test
class SelectionsTest extends TestHelper with DeclarationIndexes with TreePath {

  import global._
  
  def assertSelection(expectedTrees: String, expectedSymbols: String, src: String) = {
    
    val index = new DeclarationIndex

    val tree = treeFrom(src)
    
    index.processTree(tree)
    
    val selection = findMarkedNodes(src, tree)
    
    assertEquals(expectedTrees, selection.trees map (_.getClass.getSimpleName) mkString ", ")
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
}

