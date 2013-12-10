package scala.tools.refactoring.tests.implementations.extraction

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.implementations.extraction.ExtractParameter

class ExtractParameterTest extends TestHelper with TestRefactoring {

  def extract(name: String, extractionIdx: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractParameter with SilentTracing with TestProjectIndex
      val extraction = preparationResult.right.get.extractions(extractionIdx)
        .withAbstractionName(name)
    }
    testRefactoring.performRefactoring(testRefactoring.extraction)
  }

  @Test
  def extractSimpleParam = new FileSet {
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
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractParamToParameterlessMethod = new FileSet {
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
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractParamToMethodWithouParamList = new FileSet {
    """
      object Demo {
        def fn = {
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
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractParamToMethodWithMultipleParamLists = new FileSet {
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
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractParamToReferencedMethod = new FileSet {
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
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def expandSelection = new FileSet {
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
  }.performRefactoring(extract("extracted", 0)).assertEqualTree
}