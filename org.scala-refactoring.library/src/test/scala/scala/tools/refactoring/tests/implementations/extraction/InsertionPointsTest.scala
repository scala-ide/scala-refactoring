package scala.tools.refactoring.tests.implementations.extraction

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.implementations.extraction.InsertionPoints
import org.junit.Assert._
import scala.tools.refactoring.common.ReplaceableSelections

class InsertionPointsTest extends TestHelper with InsertionPoints with ReplaceableSelections {
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
        def atPosition(mkIp: Selection => InsertionPoint) = {
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

                def toFail = {
                  assertFalse(ip.isDefinedAt(scope))
                }
              }
          }
        }
      }
    }
  }

  @Test
  def insertInTemplate = """
    object O{
      /*(*/def fn = println(1)/*)*/
    
      def fm = println(2)
    }
    """.inScope(_.enclosingTree).atPosition(_.afterSelectionInTemplate)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn = println(1)
        
      println(123)
    
      def fm = println(2)
    }
    """)

  @Test
  def insertInBlock = """
    object O{
      def fn = {
        val a = 1
        /*(*/println(a)/*)*/
      }
    }
    """.inScope(_.enclosingTree).atPosition(_.beforeSelectionInBlock)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn = {
        val a = 1
        println(123)
        println(a)
      }
    }
    """)

  @Test
  def insertInBlockBeforeFirstDeclaration = """
    object O{
      def fn = {
        /*(*/val a = 1/*)*/
        println(a)
      }
    }
    """.inScope(_.enclosingTree).atPosition(_.beforeSelectionInBlock)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn = {
        println(123)
        val a = 1
        println(a)
      }
    }
    """)

  @Test
  def insertInBlockBeforeSelectionInSubexpression = """
    object O{
      def fn = {
        val a = 1
        println(/*(*/a/*)*/)
      }
    }
    """.inScope(_.expandTo[Block].get.selectedTopLevelTrees(0))
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

  @Test
  def insertInMethodBody = """
    object O{
      def fn(a: Int) = /*(*/println(a)/*)*/
    }
    """.inScope(_.enclosingTree).atPosition(_ => atBeginningOfDefDef)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      def fn(a: Int) = {
        println(123)
        println(a)
      }
    }
    """)

  @Test
  def insertInFunctionBody = """
    object O{
      val fn = (a: Int) => /*(*/println(a)/*)*/
    }
    """.inScope(_.enclosingTree).atPosition(_ => atBeginningOfFunction)
    .insertionOf(tprint123).shouldBecome("""
    object O{
      val fn = (a: Int) => {
        println(123)
        println(a)
      }
    }
    """)
}