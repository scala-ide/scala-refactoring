package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractValue
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._

import language.reflectiveCalls

class ExtractValueTest extends TestHelper with TestRefactoring {
  outer =>

  def extract(name: String, scopeId: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractValue with SilentTracing with TestProjectIndex
      val params = new refactoring.RefactoringParameters(
        name,
        preparationResult.right.get.potentialScopes(scopeId))
    }
    testRefactoring.performRefactoring(testRefactoring.params)
  }

  @Test
  @Ignore("tbd")
  def extractValue = new FileSet {
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
          val c = a * b
          println(c/*)*/)
        }
      }
    """
  } applyRefactoring (extract("c", 0))
}