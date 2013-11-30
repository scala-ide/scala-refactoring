package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractMethod
import tests.util.TestHelper
import tests.util.TestRefactoring

class ExtractMethodTest extends TestHelper with TestRefactoring {
  def extract(name: String, extractionIdx: Int, selectedParams: List[String])(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractMethod with SilentTracing with TestProjectIndex
      val e = preparationResult.right.get.extractions(extractionIdx)
      val params = new refactoring.RefactoringParameters(
        e,
        name,
        e.optionalParameters.filter(sym => selectedParams.contains(sym.nameString)))
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
  }.performRefactoring(extract("extracted", 0, "a" :: Nil)).assertEqualTree

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

        def fn(p: Int) = {
          for(i <- 1 to p) {
            val a = p * i
            /*(*/val (b, c, d) = extracted(p, a, na)
            println(b * b * c * d * d)
          }
        }

        def extracted(p: Int, a: Int, na: Int): (Int, Int, Int) = {
          /*(*/val b = p * a
          val c = a * na
          val d = fm(a)/*)*/
          (b, c, d)
        }

        def fm(p: Int) = p + 1
      }
    """
  }.performRefactoring(extract("extracted", 2, "na" :: Nil)).assertEqualTree
}