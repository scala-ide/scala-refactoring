package scala.tools.refactoring.tests.implementations.extraction

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.implementations.extraction.ExtractParameter

class ExtractParameterTest extends TestHelper with TestRefactoring {

  def extract(extractionIdx: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractParameter with SilentTracing with TestProjectIndex
      val extraction = preparationResult.right.get.extractions(extractionIdx)
    }
    testRefactoring.performRefactoring(testRefactoring.extraction)
  }

  @Test
  def extractSimpleParam() = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
          a * /*(*/100/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int, extracted: Int = 100) = {
          a * extracted
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractParamToParameterlessMethod() = new FileSet {
    """
      object Demo {
        def fn() = {
          10 * /*(*/100/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        def fn(extracted: Int = 100) = {
          10 * extracted
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test(expected=classOf[IndexOutOfBoundsException])
  def dontExtractParamToMethodWithouParamList() = new FileSet {
    """
      object Demo {
        def fn = {
          10 * /*(*/100/*)*/
        }
      }
    """ becomes
      """"""
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractParamToMethodWithMultipleParamLists() = new FileSet {
    """
      object Demo {
        def fn(a: Int)(b: Int) = {
          /*(*/100/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int)(b: Int, extracted: Int = 100) = {
          extracted
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractParamToReferencedMethod() = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
          /*(*/100/*)*/
        }

	  	fn(1)
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int, extracted: Int = 100) = {
          extracted
        }

        fn(1)
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test(expected = classOf[IndexOutOfBoundsException])
  def dontExtractWithInaccessibleDependencies() = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
	      val b = 100
          /*(*/b/*)*/
        }

	  	fn(1)
      }
    """ becomes
      """"""
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  @Ignore
  def expandSelection() = new FileSet {
    """
      object Demo {
        def fn(a: Int) = {
          a + "string"/*<-*/
        }
      }
    """ becomes
      """
      object Demo {
        def fn(a: Int, extracted: String = "string") = {
          a + extracted
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def avoidNameCollisions = new FileSet {
    """
      object Demo {
        def fn() = {
          val extracted = /*(*/1/*)*/
          extracted
        }
      }
    """ becomes
      """
      object Demo {
        def fn(extracted1: Int = 1) = {
          val extracted = extracted1
          extracted
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
}