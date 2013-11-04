package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractValue
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._
import language.reflectiveCalls
import scala.tools.refactoring.implementations.extraction.ExtractionScopes

class ExtractValueTest extends TestHelper with TestRefactoring with ExtractionScopes {
  outer =>

  def extract(name: String, p: ExtractionScopePredicate)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractValue with SilentTracing with TestProjectIndex
      val pred = p.asInstanceOf[refactoring.ExtractionScopePredicate]
      val params = new refactoring.RefactoringParameters(
        name,
        preparationResult.right.get.potentialScopes.filter(pred(_)).head)
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
  } applyRefactoring (extract("c", isA[BlockScope]))

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
  } applyRefactoring (extract("c", isA[BlockScope]))

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
  } applyRefactoring (extract("c", isA[TemplateScope]))
}