package scala.tools.refactoring.tests

import scala.tools.refactoring.util.Tracing
import scala.tools.refactoring.util.SilentTracing
import scala.tools.refactoring.OrganizeImports
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class OrganizeImportsTest extends TestHelper {
  
  class StringExtractMethod(source: String) {
    def organize(expected: String) = {
      val refactoring = new OrganizeImports(global) with Tracing
      refactoring.prepare(compile(source), 0, 0) match {
        case Right(prepare) =>
          val result = refactoring.perform(prepare, new refactoring.RefactoringParameters) match {
            case Right(result) => result
            case Left(error) => error
          }
          assertEquals(expected, result)
        case Left(error) => fail()
      }
    }
  }
  
  implicit def stringToStringExtractMethod(source: String) = new StringExtractMethod(source)

  @Test
  def organize = """
    import java.lang.String
    import scala.collection.mutable.ListBuffer
    import java.lang.Object

    object Main
    """ organize(
    """
    import java.lang.{Object, String}
    import scala.collection.mutable.ListBuffer

    object Main
    """)
}
