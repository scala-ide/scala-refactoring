package scala.tools.refactoring.tests.common

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.common.Occurrences
import org.junit.Assert._
import scala.tools.refactoring.analysis.GlobalIndexes

class OccurrencesTest extends TestHelper with GlobalIndexes with Occurrences {
  import global._

  var index: IndexLookup = null

  def withIndex(src: String)(body: Tree => Unit) {
    val tree = treeFrom(src)
    global.ask { () =>
      index = GlobalIndex(List(CompilationUnitIndex(tree)))
    }
    body(tree)
  }

  @Test
  def termOccurrences() = {
    val src = """
      object O{
        def fn = {
          val a = 1
    	  val b = {
            val a = 2
            a
          }
          a * a
        }
      }
      """
    withIndex(src) { root =>
      val os = global.ask { () => termNameOccurrences(root, "a") }
      assertEquals(src.indexOf("a = 1"), os.head._1)
      assertTrue(os.forall(_._2 == "a".length))
      assertEquals(3, os.length)
    }
  }

  @Test
  def accessorNameOccurrences() = {
    val src = """
      object O{
        val field = 1

        def fn = 2 * field
      }
      """
    withIndex(src) { root =>
      val os = global.ask { () => termNameOccurrences(root, "field") }
      assertEquals(src.indexOf("field"), os.head._1)
      assertTrue(os.forall(_._2 == "field".length))
      assertEquals(2, os.length)
    }
  }

  @Test
  def paramsOccurrences() = {
    val src = """
      object O{
        def fn(a: Int, b: Int) = {
          val c = a * b
          a * b * c
        }
      }
      """
    withIndex(src) { root =>
      val os = global.ask { () => defDefParameterOccurrences(root, "fn") }
      val aos = os(0)
      val bos = os(1)
      assertEquals(src.indexOf("a: Int"), aos.head._1)
      assertEquals(src.indexOf("b: Int"), bos.head._1)
      assertTrue((aos ::: bos).forall(_._2 == 1))
      // two params
      assertEquals(2, os.length)
      // each name occurs three times
      assertEquals(aos.length, 3)
      assertEquals(bos.length, 3)
    }
  }
}