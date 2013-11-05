package scala.tools.refactoring.tests.common

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.common.ReplaceableSelections
import org.junit.Assert._

class ReplaceableSelectionTest extends TestHelper with ReplaceableSelections {
  import global._

  val t123 = Literal(Constant(123))
  val tprint123 =
    "object O { /*(*/println(123)/*)*/ }".selection.selectedTopLevelTrees.head

  implicit class StringToSel(src: String) {
    val root = treeFrom(src)
    val selection = {
      val start = commentSelectionStart(src)
      val end = commentSelectionEnd(src)
      FileSelection(root.pos.source.file, root, start, end)
    }

    def assertReplacement(mkTrans: Selection => Transformation[Tree, Tree]) = {
      val trans = mkTrans(selection)
      val result = trans(root)

      new {
        def toFail =
          assertTrue(result.isEmpty)

        def toBecome(expectedSrc: String) = {
          val (expected, actual) = global.ask { () =>
            (expectedSrc.root.toString(), result.get.toString())
          }
          assertEquals(expected, actual)
        }
      }
    }
  }

  @Test
  def replaceSingleStatement = """
    object O{
      def f = /*(*/1/*)*/
    }
    """.assertReplacement(_.replaceBy(t123)).toBecome("""
    object O{
      def f = 123
    }
    """)

  @Test
  def replaceSingleStatementInArgument = """
    object O{
      println(/*(*/1/*)*/)
    }
    """.assertReplacement(_.replaceBy(t123)).toBecome("""
    object O{
      println(123)
    }
    """)

  @Test
  def replaceSequence = """
    object O{
      def f = {
        /*(*/println(1)
        println(2)/*)*/
        println(3)
      }
    }
    """.assertReplacement(_.replaceBy(t123)).toBecome("""
    object O{
      def f = {
        123
        println(3)
      }
    }
    """)

  @Test
  def replaceAllExpressionsInBlock = """
    object O{
      def f = {
        /*(*/println(1)
        println(2)
        println(3)/*)*/
      }
    }
    """.assertReplacement(_.replaceBy(tprint123)).toBecome("""
    object O{
      def f = println(123)
    }
    """)
}