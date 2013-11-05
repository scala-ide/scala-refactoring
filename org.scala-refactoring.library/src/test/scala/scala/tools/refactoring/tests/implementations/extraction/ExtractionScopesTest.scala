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
    val extractionPoint =
      selection.beforeSelectionInBlock orElse
        selection.afterSelectionInTemplate orElse
        atBeginningOfDefDef orElse
        atBeginningOfFunction

    def findFirstScope(f: ExtractionScope.Filter) = {
      val scope = collectExtractionScopes(selection, extractionPoint, f).head

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
    """.findFirstScope(ExtractionScope.isA[TemplateScope]).assertInsertion(_.insert(tprint123)).toBecome("""
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
    """.findFirstScope(ExtractionScope.isA[BlockScope]).assertInsertion(_.insert(tprint123)).toBecome("""
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
    """.findFirstScope(ExtractionScope.isA[BlockScope]).assertInsertion(_.insert(tprint123)).toBecome("""
    object O{
      def fn = {
        println(123)
        val a = 1
        println(a)
      }
    }
    """)

  @Test
  def insertInMethodBody = """
    object O{
      def fn(a: Int) = /*(*/println(a)/*)*/
    }
    """.findFirstScope(ExtractionScope.isA[MethodScope]).assertInsertion(_.insert(tprint123)).toBecome("""
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
    """.findFirstScope(ExtractionScope.isA[FunctionScope]).assertInsertion(_.insert(tprint123)).toBecome("""
    object O{
      val fn = (a: Int) => {
        println(123)
        println(a)
      }
    }
    """)
}