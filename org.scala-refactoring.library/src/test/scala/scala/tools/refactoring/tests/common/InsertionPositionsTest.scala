package scala.tools.refactoring.tests.common

import scala.tools.refactoring.common.InsertionPositions
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.tests.util.TestHelper

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

class InsertionPositionsTest extends TestHelper with InsertionPositions with Selections {
  import global._

  val t123 = Literal(Constant(123))
  val tprint123 =
    "object O { /*(*/println(123)/*)*/ }".selection.selectedTopLevelTrees.head

  implicit class StringToSelection(src: String) {
    val root = treeFrom(src)
    val selection = {
      val start = commentSelectionStart(src)
      val end = commentSelectionEnd(src)
      FileSelection(root.pos.source.file, root, start, end)
    }

    def inScope(mkScope: Selection => Tree) = {
      val scope = mkScope(selection)
      new {
        def atPosition(mkIp: Selection => InsertionPosition) = {
          val ip = mkIp(selection)
          new {
            def insertionOf(inserted: Tree) =
              new {
                def shouldBecome(expectedSrc: String) = {
                  assertTrue(ip.isDefinedAt(scope))
                  val trans = topdown {
                    matchingChildren {
                      transform {
                        case t if t.samePosAndType(scope) => ip(scope)(inserted)
                      }
                    }
                  }
                  val result = trans(selection.root).get
                  val (expected, actual) = global.ask { () => (treeFrom(expectedSrc).toString, result.toString) }
                  assertEquals(expected, actual)
                }

                def toFail() = {
                  assertFalse(ip.isDefinedAt(scope))
                }
              }
          }
        }
      }
    }
  }

  @Test
  def insertInTemplate() = global.ask { () => """
    object O{
      /*(*/def fn = println(1)/*)*/

      def fm = println(2)
    }
    """.inScope(_.expandTo[Template].get.enclosingTree).atPosition(_.afterSelectionInTemplate)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn = println(1)

      println(123)

      def fm = println(2)
    }
    """)
  }

  @Test
  def insertInBlock() = global.ask { () => """
    object O{
      def fn = {
        val a = 1
        /*(*/println(a)/*)*/
      }
    }
    """.inScope(_.expandTo[Block].get.enclosingTree).atPosition(_.beforeSelectionInBlock)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn = {
        val a = 1
        println(123)
        println(a)
      }
    }
    """)
  }

  @Test
  def insertInBlockBeforeFirstDeclaration() = global.ask { () => """
    object O{
      def fn = {
        /*(*/val a = 1/*)*/
        println(a)
      }
    }
    """.inScope(_.expandTo[Block].get.enclosingTree).atPosition(_.beforeSelectionInBlock)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn = {
        println(123)
        val a = 1
        println(a)
      }
    }
    """)
  }

  @Test
  def insertInBlockBeforeSelectionInSubexpression() = global.ask { () => """
    object O{
      def fn = {
        val a = 1
        println(/*(*/a/*)*/)
      }
    }
    """.inScope(_.expandTo[Block].get.enclosingTree)
    .atPosition(_.beforeSelectionInBlock)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn = {
        val a = 1
        println(123)
        println(a)
      }
    }
    """)
  }

  @Test
  def insertInMethodBody() = global.ask { () => """
    object O{
      def fn(a: Int) = /*(*/println(a)/*)*/
    }
    """.inScope(_.expandTo[DefDef].get.enclosingTree).atPosition(_ => atBeginningOfNewDefBody)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn(a: Int) = {
        println(123)
        println(a)
      }
    }
    """)
  }

  @Test
  def insertInFunctionBody() = global.ask { () => """
    object O{
      val fn = (a: Int) => /*(*/println(a)/*)*/
    }
    """.inScope(_.expandTo[Function].get.enclosingTree).atPosition(_ => atBeginningOfNewFunctionBody)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      val fn = (a: Int) => {
        println(123)
        println(a)
      }
    }
    """)
  }

  @Test
  def insertInCaseBody() = global.ask { () => """
    object O{
      val i = 1 match {
        case i: Int => /*(*/i/*)*/
      }
    }
    """.inScope(_.expandTo[CaseDef].get.enclosingTree).atPosition(_ => atBeginningOfCaseBody)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      val i = 1 match {
        case i: Int =>
          println(123)
          i
      }
    }
    """)
  }
}