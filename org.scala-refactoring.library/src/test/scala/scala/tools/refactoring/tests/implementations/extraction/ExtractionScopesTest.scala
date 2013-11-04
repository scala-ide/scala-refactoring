package scala.tools.refactoring.tests.implementations.extraction

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.implementations.extraction.ExtractionScopes
import org.junit.Assert._

class ExtractionScopesTest extends TestHelper with ExtractionScopes {
  import global._

  val t123 = Literal(Constant(123))
  val tprint123 =
    "object O { /*(*/println(123)/*)*/ }".selection.selectedTopLevelTrees.head

  implicit class StringToScopes(src: String) {
    val root = treeFrom(src)
    val selection = {
      val start = commentSelectionStart(src)
      val end = commentSelectionEnd(src)
      FileSelection(root.pos.source.file, root, start, end)
    }

    def findFirstScope(p: ExtractionScopePredicate) = {
      val scope = collectExtractionScopes(selection, p).head

      new {
        def assertInsertion(mkTrans: ExtractionScope => Transformation[Tree, Tree]) = {
          val trans = mkTrans(scope)
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
    }
  }
  
  @Test
  def insertInTemplate = """
    object O{
      def fn = /*(*/println(1)/*)*/
    
      def fm = println(2)
    }
    """.findFirstScope(isA[TemplateScope]).assertInsertion(_.insert(tprint123)).toBecome("""
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
    """.findFirstScope(isA[BlockScope]).assertInsertion(_.insert(tprint123)).toBecome("""
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
    """.findFirstScope(isA[MethodScope]).assertInsertion(_.insert(tprint123)).toBecome("""
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
    """.findFirstScope(isA[FunctionScope]).assertInsertion(_.insert(tprint123)).toBecome("""
    object O{
      val fn = (a: Int) => {
        println(123)
        println(a)
      }
    }
    """)
}