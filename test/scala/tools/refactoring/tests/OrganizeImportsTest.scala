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
      val refactoring = new OrganizeImports(global) with SilentTracing
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
  def sort = """
    import scala.collection.mutable.ListBuffer
    import java.lang.Object

    object Main
    """ organize(
    """
    import java.lang.Object
    import scala.collection.mutable.ListBuffer

    object Main
    """)
    
  @Test
  def collapse = """
    import java.lang.String
    import java.lang.Object

    object Main
    """ organize(
    """
    import java.lang.{Object, String}

    object Main
    """)    
    
  @Test
  def sortAndCollapse = """
    import scala.collection.mutable.ListBuffer
    import java.lang.String
    import java.lang.Object

    object Main
    """ organize(
    """
    import java.lang.{Object, String}
    import scala.collection.mutable.ListBuffer

    object Main
    """)    
    
  @Test
  def collapseWithRename = """
    import java.lang.{String => S}
    import java.lang.{Object => O}

    object Main
    """ organize(
    """
    import java.lang.{Object => O, String => S}

    object Main
    """)     
    
  @Test
  def importAll = """
    import java.lang._
    import java.lang.String

    object Main
    """ organize(
    """
    import java.lang._

    object Main
    """)      
    
  @Test
  def importOnTrait = """
    import java.lang._
    import java.lang.String

    trait A

    trait Main extends A {
    }
    """ organize(
    """
    import java.lang._

    trait A

    trait Main extends A {
    }
    """)    
    
  @Test
  def importWithSpace = """

    import scala.collection.mutable.ListBuffer
    import java.lang.String

    object Main
    """ organize(
    """

    import java.lang.String
    import scala.collection.mutable.ListBuffer

    object Main
    """)    
    
  @Test
  def importAllWithRename = """
    import java.lang._
    import java.lang.{String => S}

    object Main
    """ organize(
    """
    import java.lang.{String => S, _}

    object Main
    """)
}
