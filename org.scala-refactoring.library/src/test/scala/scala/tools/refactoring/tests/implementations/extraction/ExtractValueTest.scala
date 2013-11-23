package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractValue
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._
import scala.tools.refactoring.analysis.VisibilityScopes

class ExtractValueTest extends TestHelper with TestRefactoring with VisibilityScopes {
  outer =>

  def extract(name: String, f: VisibilityScope => Boolean)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractValue with SilentTracing with TestProjectIndex
      val params = refactoring.RefactoringParameters(
        preparationResult.right.get.extractions.filter(e => f(e.scope.asInstanceOf[VisibilityScope])).head,
        name)
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
  }.performRefactoring(extract("c", _.isInstanceOf[BlockScope])).assertEqualTree

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
  }.performRefactoring(extract("c", _.isInstanceOf[BlockScope])).assertEqualTree

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
          /*(*/val b = c/*)*/
          println(b)
        }
      }
    """
  }.performRefactoring(extract("c", _.isInstanceOf[BlockScope])).assertEqualTree

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
          /*(*/val (c, d) = e
          println(c * d)
        }
      }
    """
  }.performRefactoring(extract("e", _.isInstanceOf[BlockScope])).assertEqualTree

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
        val c = /*(*/1 + 2

        def fn(a: Int) = {
          val b = c/*)*/
        }

        def fm = 7
      }
    """
  }.performRefactoring(extract("c", _.isInstanceOf[TemplateScope])).assertEqualTree
}