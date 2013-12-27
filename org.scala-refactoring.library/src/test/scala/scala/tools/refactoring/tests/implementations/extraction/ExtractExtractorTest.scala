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
	  	  case Extracted(i) => println(i)
        }
    
        object Extracted {
          def unapply(x: Int): Option[Int] = x match {
    		case i => Some(i)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractorWithMultipleBindings = new FileSet{
    """
      object Demo {
        (1, 2) match {
	  	  case /*(*/(x, y)/*)*/ => println(x*y)
        }
      }
    """ becomes
      """
      object Demo {
        (1, 2) match {
	  	  case Extracted(x, y) => println(x*y)
        }
    
        object Extracted {
          def unapply(x: (Int, Int)): Option[(Int, Int)] = x match {
    		case (x, y) => Some(x, y)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractorWithoutBindings = new FileSet{
    """
      object Demo {
        1 match {
	  	  case /*(*/1/*)*/ => println(1)
        }
      }
    """ becomes
      """
      object Demo {
        1 match {
	  	  case Extracted() => println(1)
        }
    
        object Extracted {
          def unapply(x: Int): Option[Unit] = x match {
    		case 1 => Some()
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractSubPattern = new FileSet{
    """
      object Demo {
        1 match {
	  	  case /*(*/1/*)*/ | 2 => println(1)
        }
      }
    """ becomes
      """
      object Demo {
        1 match {
	  	  case Extracted() | 2 => println(1)
        }
    
        object Extracted {
          def unapply(x: Int): Option[Unit] = x match {
    		case 1 => Some()
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
}