package scala.tools.refactoring.tests.implementations.extraction

import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.implementations.extraction.ExtractExtractor

class ExtractExtractorTest extends TestHelper with TestRefactoring {
  def extract(extractionIdx: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractExtractor with TestProjectIndex
      val extraction = preparationResult.right.get.extractions(extractionIdx).asInstanceOf[refactoring.Extraction]
    }
    testRefactoring.performRefactoring(testRefactoring.extraction)
  }

  @Test
  def extractSimpleExtractor() = new FileSet {
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
          def unapply(x: Int) = x match {
            case i => Some(i)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractWithBodyOnNextLine() = new FileSet {
    """
      object Demo {
        List(1) match {
          case /*(*/List(i)/*)*/ =>
            println(i)
        }
      }
    """ becomes
      """
      object Demo {
        List(1) match {
          case Extracted(i) =>
            println(i)
        }
    
        object Extracted {
          def unapply(x: List[Int]) = x match {
            case List(i) => Some(i)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree()

  @Test
  def extractWithBodyOnNewLine() = new FileSet {
    """
      object Demo {
        List(1) match {
          case /*(*/List(i)/*)*/ => 
            println(i)
        }
      }
    """ becomes
      """
      object Demo {
        List(1) match {
          case Extracted(i) => println(i)
        }
    
        object Extracted {
          def unapply(x: List[Int]) = x match {
            case List(i) => Some(i)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractorWithMultipleBindings() = new FileSet{
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
          def unapply(x: (Int, Int)) = x match {
            case (x, y) => Some(x, y)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractorWithoutBindings() = new FileSet{
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
          def unapply(x: Int) = x match {
            case 1 => true
            case _ => false
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractSubPattern() = new FileSet{
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
          def unapply(x: Int) = x match {
            case 1 => true
            case _ => false
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractPatternWithGuard() = new FileSet{
    """
      object Demo {
        1 match {
          case /*(*/i if i < 10/*)*/ => println(1)
        }
      }
    """ becomes
      """
      object Demo {
        1 match {
          case Extracted(i) => println(1)
        }
    
        object Extracted {
          def unapply(x: Int) = x match {
            case i if i < 10 => Some(i)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractWithoutGuard() = new FileSet{
    """
      object Demo {
        "abc" match {
          case /*(*/s/*)*/ if s.length < 10 => println(s)
        }
      }
    """ becomes
      """
      object Demo {
        "abc" match {
          case Extracted(s) if s.length < 10 => println(s)
        }
    
        object Extracted {
          def unapply(x: String) = x match {
            case s => Some(s)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractListPattern() = new FileSet{
    """
      object Demo {
        List(1, 2, 3) match {
          case /*(*/1 :: rest/*)*/ => println(rest)
        }
      }
    """ becomes
      """
      object Demo {
        List(1, 2, 3) match {
          case Extracted(rest) => println(rest)
        }
    
        object Extracted {
          def unapply(x: List[Int]) = x match {
            case 1 :: rest => Some(rest)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractConstructorPatternWithoutGuard() = new FileSet{
    """
      case class TwoInts(a: Int, b: Int)
      object Demo {
        TwoInts(1, 2) match {
          case /*(*/TwoInts(a, b)/*)*/ if a * b == 42 => ()
        }
      }
    """ becomes
      """
      case class TwoInts(a: Int, b: Int)
      object Demo {
        TwoInts(1, 2) match {
          case /*(*/Extracted(a, b)/*)*/ if a * b == 42 => ()
        }
    
        object Extracted {
          def unapply(x: TwoInts) = x match {
            case TwoInts(a, b) => Some(a, b)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def avoidNameCollisions = new FileSet{
    """
      object Extracted {
        1 match {
          case /*(*/i: Int/*)*/ => println(i)
        }
      }
    """ becomes
      """
      object Extracted {
        1 match {
          case Extracted1(i) => println(i)
        }
    
        object Extracted1 {
          def unapply(x: Int) = x match {
            case i => Some(i)
            case _ => None
          }
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree
  
  @Test
  def extractToPackage = new FileSet{
    """
      object O {
        1 match {
          case /*(*/i: Int/*)*/ => println(i)
        }
      }
    """ becomes
      """
      object O {
        1 match {
          case Extracted(i) => println(i)
        }
      }

      object Extracted {
        def unapply(x: Int) = x match {
          case i => Some(i)
          case _ => None
        }
      }
    """
  }.performRefactoring(extract(1)).assertEqualTree
}
