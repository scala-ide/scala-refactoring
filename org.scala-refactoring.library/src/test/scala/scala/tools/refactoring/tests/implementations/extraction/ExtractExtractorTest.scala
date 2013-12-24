package scala.tools.refactoring.tests.implementations.extraction

import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.implementations.extraction.ExtractExtractor

class ExtractExtractorTest extends TestHelper with TestRefactoring {
  def extract(extractionIdx: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractExtractor with SilentTracing with TestProjectIndex
      val extraction = preparationResult.right.get.extractions(extractionIdx).asInstanceOf[refactoring.Extraction]
    }
    testRefactoring.performRefactoring(testRefactoring.extraction)
  }

  @Test
  def extractSimpleExtractor = new FileSet {
    """
      object Demo {
        1 match {
	  	  case /*(*/i: Int/*)*/ => println(i)
        }
      }
    """ becomes
      """
      object Demo {
        1 match {
	  	  case i: Int => println(i)
        }
    
        object Extracted {
          def unapply(x: Int): Option[Int] = x match {
    		case i: Int => Some(i)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
}