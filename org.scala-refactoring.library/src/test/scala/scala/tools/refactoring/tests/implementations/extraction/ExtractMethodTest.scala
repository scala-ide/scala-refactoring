package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractMethod
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._
import scala.tools.refactoring.analysis.VisibilityScopes

class ExtractMethodTest extends TestHelper with TestRefactoring with VisibilityScopes {
  def extract(name: String, f: VisibilityScope => Boolean, selectedParams: List[String])(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractMethod with SilentTracing with TestProjectIndex
      val scope = preparationResult.right.get.possibleExtractions.filter(e => f(e.scope.asInstanceOf[VisibilityScope])).head
      val params = new refactoring.RefactoringParameters(
        name,
        scope,
        scope.definedDependencies.filter(sym => selectedParams.contains(sym.nameString)))
    }
    testRefactoring.performRefactoring(testRefactoring.params)
  }

  @Test
  def extractSimpleMethod = new FileSet {
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
          def extracted(a: Int): Int = {
            /*(*/a * b
          }
          println(/*(*/extracted(a)/*)*/)
        }
      }
    """
  }.performRefactoring(extract("extracted", v => v.isInstanceOf[BlockScope], "a" :: Nil)).assertEqualTree

  @Test
  def extractComplexMethod = new FileSet {
    """
      object Demo {
        val na = 1

        def fn(p: Int) = {
          for(i <- 1 to p) {
            val a = p * i
            /*(*/val b = p * a
            val c = a * na
            val d = fm(a)/*)*/
            println(b * b * c * d * d)
          }
        }

        def fm(p: Int) = p + 1
      }
    """ becomes
      """
      object Demo {
        val na = 1

        def extracted(p: Int, a: Int, na: Int): (Int, Int, Int) = {
          /*(*/val b = p * a
          val c = a * na
          val d = fm(a)/*)*/
          (b, c, d)
        }

        def fn(p: Int) = {
          for(i <- 1 to p) {
            val a = p * i
            /*(*/val (b, c, d) = extracted(p, a, na)
            println(b * b * c * d * d)
          }
        }

        def fm(p: Int) = p + 1
      }
    """
  }.performRefactoring(extract("extracted", v => v.isInstanceOf[TemplateScope], "na" :: Nil)).assertEqualTree
}