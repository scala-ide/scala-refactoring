package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractCode
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._
import scala.tools.refactoring.analysis.VisibilityScopes

class ExtractCodeTest extends TestHelper with TestRefactoring with VisibilityScopes {
  def extract(name: String, f: VisibilityScope => Boolean, selectedParams: List[String])(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractCode with SilentTracing with TestProjectIndex
      val scope = preparationResult.right.get.possibleExtractions.filter(e => f(e.scope.asInstanceOf[VisibilityScope])).head
      val params = new refactoring.RefactoringParameters(
        name,
        scope,
        scope.definedDependencies.filter(sym => selectedParams.contains(sym.nameString)))
    }
    testRefactoring.performRefactoring(testRefactoring.params)
  }

  @Test
  def extractCodeWithoutUnknownDependencies = new FileSet {
    """
      object Demo {
        val a = 1
        val b = /*(*/a * a/*)*/
      }
    """ becomes
      """
      object Demo {
        val a = 1
        val b = extracted/*)*/

        val extracted = /*(*/a * a
      }
    """
  } applyRefactoring (extract("extracted", _.isInstanceOf[TemplateScope], Nil))

  @Test
  def extractCodeWithoutUnknownDependenciesAndParams = new FileSet {
    """
      object Demo {
        val a = 1
        val b = /*(*/a * a/*)*/
      }
    """ becomes
      """
      object Demo {
        val a = 1
        val b = /*(*/extracted(a)/*)*/

        def extracted(a: Int): Int = {
          /*(*/a * a
        }
      }
    """
  } applyRefactoring (extract("extracted", _.isInstanceOf[TemplateScope], "a" :: Nil))

  @Test
  def extractCodeWithUnknownDependencies = new FileSet {
    """
      object Demo {
        val a = 1
        val b = {
          val c = 2
          /*(*/a * c/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        val a = 1
        val b = {
          val c = 2
          /*(*/extracted(c)/*)*/
        }

        def extracted(c: Int): Int = {
          /*(*/a * c/*)*/
        }
      }
    """
  } applyRefactoring (extract("extracted", _.isInstanceOf[TemplateScope], Nil))

  @Test
  def extractMultipleExpressions = new FileSet {
    """
      object Demo {
        val a = 1
        val b = {
          /*(*/val c = 2
          val d = a
          d * c/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        val a = 1
        val b = {
          val extracted = {
                 /*(*/val c = 2
                 val d = a
                 d * c/*)*/
               }
          extracted
        }
      }
    """
  } applyRefactoring (extract("extracted", _.isInstanceOf[BlockScope], Nil))
}