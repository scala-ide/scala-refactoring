package scala.tools.refactoring.tests.analysis

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.analysis.VisibilityScopes
import org.junit.Assert._
import sun.security.util.Length

class VisibilityScopesTest extends TestHelper with VisibilityScopes {
  import global._
  
  case class Expectation(scope: String, children: List[Expectation]){
    override def toString =
      scope + children.mkString("{ ", ", ", " }")
  }

  def selectionSees(expected: => Expectation) = expected
  
  implicit def toExpectation(scope: String) =
    Expectation(scope, Nil)
  
  implicit class ExpectationBuilder(scope: String){
    def sees(childScope: Expectation) =
      Expectation(scope, childScope :: Nil)
  }

  def findMarkedNodes(src: String, tree: Tree) = {
    val start = commentSelectionStart(src)
    val end = commentSelectionEnd(src)
    FileSelection(tree.pos.source.file, tree, start, end)
  }

  def assertVisibilities(expected: Expectation)(src: String) = {
    val tree = treeFrom(src)
    val selection = findMarkedNodes(src, tree)
    val vt = selection.visibilityScope

    assertEquals(expected.toString(), vt.toString())
  }

  @Test
  def testVisibility = assertVisibilities(
    selectionSees {
      "Block(value c)" sees {
        "Template(constructor A, value a, value b)" sees {
          "PackageDef(class A)"
        }
      }
    }) {
      """
class A{
  val a = 1
  val b = {
    val c = 3
    /*(*/c/*)*/
  }
}
"""
    }

  @Test
  def visibilityOfOuterTypes = assertVisibilities(
    selectionSees {
      "Template(constructor A)" sees {
        "PackageDef(object O, class A, trait T)"
      }
    })(
      """
object O{
  val invisible = 1    
}
	        
class A{
  /*(*/println(1)/*)*/
}
	        
trait T
""")

  @Test
  def visibilityOfMethods = assertVisibilities(
    selectionSees {
      "Template(constructor O, method a, method b, method c)" sees {
        { "PackageDef(object O)" }
      }
    })(
      """
object O{
  def a = 1
  def b = {
    /*(*/println(1)/*)*/
  }
  def c = 3
}
""")

  @Test
  def visibilityOfMethodParameters = assertVisibilities(
    selectionSees {
      "Block(value c)" sees {
        "DefDef(value a, value b)" sees {
          "Template(constructor O, method b)" sees {
            "PackageDef(object O)"
          }
        }
      }
    })(
      """
object O{
  def b(a: Int, b: Int) = {
    val c = a * b
    /*(*/println(c)/*)*/
  }
}
""")

  @Test
  def visibilityOfConstructorParameters = assertVisibilities(
    selectionSees {
      "Template(value a, value b, constructor A)" sees {
        "PackageDef(class A)"
      }
    })(
      """
class A(a: Int, b: Int){
  /*(*/println(a * b)/*)*/
}
""")

  @Test
  def visibilityInBlocks = assertVisibilities(
    selectionSees {
      "Block(value d)" sees {
        "Block(value b)" sees {
          "Block(value a)" sees {
            "Template(constructor A)" sees { "PackageDef(class A)" }
          }
        }
      }
    })(
      """
class A{
  {
    val a = 1
    val b = {
      val c = 2
      println(c)
    }
    val d = 3
    /*(*/println(a * b * d)/*)*/
  }
}
""")

  @Test
  def visibilityInForComprehensions = assertVisibilities(
    selectionSees {
      "Block(value p)" sees {
        "Function(value j)" sees {
          "Function(value i)" sees {
            "Template(constructor A)" sees { "PackageDef(class A)" }
          }
        }
      }
    })(
      """
class A{
  for(i <- 1 to 10; j <- 1 to 10){
    val p = i * j      
    /*(*/println(p)/*)*/
  }
}
""")

  @Test
  def visibilityInForEnumerator = assertVisibilities(
    selectionSees {
      "Function(value j)" sees {
        "Function(value i)" sees {
          "Template(constructor A)" sees { "PackageDef(class A)" }
        }
      }
    })(
      """
class A{
  for{
    i <- 1 to 10
    j <- /*(*/List(1, 2, 3)/*)*/
  }{   
    println(i * j)
  }
}
""")
}