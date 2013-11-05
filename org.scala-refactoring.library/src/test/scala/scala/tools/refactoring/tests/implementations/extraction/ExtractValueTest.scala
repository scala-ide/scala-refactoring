package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractValue
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._
import scala.tools.refactoring.implementations.extraction.ExtractionScopes

class ExtractValueTest extends TestHelper with TestRefactoring with ExtractionScopes {
  outer =>

  def extract(name: String, f: ExtractionScope.Filter)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractValue with SilentTracing with TestProjectIndex
      val params = new refactoring.RefactoringParameters(
        name,
        preparationResult.right.get.potentialScopes.filter(s => f.isDefinedAt(s.asInstanceOf[ExtractionScope])).head)
    }
    testRefactoring.performRefactoring(testRefactoring.params)
  }

  @Test
  def extractSimpleValue = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
          val b = 2
          println(/*(*/a * b/*)*/)
        }
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int) = {
          val b = 2
          val c = /*(*/a * b
          println(c/*)*/)
        }
      }
    """
  } applyRefactoring (extract("c", ExtractionScope.isA[BlockScope]))

  @Test
  def extractSimpleSequence = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
          val b = 2
          /*(*/println(a)
          println(b)/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int) = {
          val b = 2
          val c = {
            /*(*/println(a)
            println(b)/*)*/
          }
          c
        }
      }
    """
  } applyRefactoring (extract("c", ExtractionScope.isA[BlockScope]))

  @Test
  def extractWithOutboundDependency = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
          /*(*/val b = a + 1/*)*/
          println(b)
        }
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int) = {
          val c = {
            /*(*/val b = a + 1/*)*/
            b
          }
          val b = c
          println(b)
        }
      }
    """
  } applyRefactoring (extract("c", ExtractionScope.isA[BlockScope]))

  @Test
  def extractWithOutboundDependencies = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
          /*(*/val b = a + 1
          val c = a + 2
          println(b*c)
          val d = a + 3/*)*/
          println(c * d)
        }
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int) = {
          val e = {
            /*(*/val b = a + 1
            val c = a + 2
            println(b*c)
            val d = a + 3/*)*/
            (c, d)
          }
          val (c, d) = e
          println(c * d)
        }
      }
    """
  } applyRefactoring (extract("e", ExtractionScope.isA[BlockScope]))

  @Test
  def extractToTemplateScope = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
          val b = /*(*/1 + 2/*)*/
        }

        def fm = 7
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int) = {
          val b = c/*)*/
        }

        val c = /*(*/1 + 2

        def fm = 7
      }
    """
  } applyRefactoring (extract("c", ExtractionScope.isA[TemplateScope]))
}