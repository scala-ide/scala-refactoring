package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractCode
import tests.util.TestHelper
import tests.util.TestRefactoring

class ExtractCodeTest extends TestHelper with TestRefactoring {
  def extract(name: String, extractionIdx: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractCode with SilentTracing with TestProjectIndex
      val e = preparationResult.right.get.extractions(extractionIdx)
        .withAbstractionName(name)
    }
    testRefactoring.performRefactoring(testRefactoring.e)
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
        val b = extracted
        val extracted = a * a
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

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
          extracted(c)
        }

        def extracted(c: Int): Int = {
          a * c
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree

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
            val c = 2
            val d = a
            d * c
          }
          extracted
        }
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractUnitExpressionToDef = new FileSet {
    """
      object Demo {
        /*(*/println("hello world")/*)*/
      }
    """ becomes
      """
      object Demo {
        extracted
    
        def extracted(): Unit = {
          println("hello world")
        }
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractCodeInCase = new FileSet {
    """
      object Demo {
        val p = (1, 1) match {
          case (x: Int, y: Int) => /*(*/x * y/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        val p = (1, 1) match {
          case (x: Int, y: Int) => 
            val extracted = x * y
            extracted
        }
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  @Ignore("Todo...")
  def extractFunction = new FileSet {
    """
      object Demo {
        val a = (1 to 10).map{ /*(*/i =>
          i + 100/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        val a = (1 to 10).map(extracted)
        val extracted = (i: Int) => i + 100
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractCodeWithPotentialSideEffects = new FileSet {
    """
      object Demo {
        val a = {
          /*(*/println("calculate answer...")
          6 * 7/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        val a = extracted
    
        def extracted(): Int = {
          println("calculate answer...")
          6 * 7
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree

  @Test
  def extractCodeWithPotentialSideEffectsOnVar = new FileSet {
    """
      object Demo {
        var c = 1
        val a = {
          /*(*/c += 1
          6 * 7/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        var c = 1
    
        val a = extracted
    
        def extracted(): Int = {
          c += 1
          6 * 7
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree

  @Test
  def extractForEnumerator = new FileSet {
    """
      object Demo {
        for{
          i <- /*(*/1 to 100/*)*/
        } println(i)
      }
    """ becomes
      """
      object Demo {
        for{
          i <- extracted
        } println(i)
    
        val extracted = 1 to 100
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractFromForBody = new FileSet {
    """
      object Demo {
        for{
          i <- 1 to 100
        } /*(*/println(i)/*)*/
      }
    """ becomes
      """
      object Demo {
        for{
          i <- 1 to 100
        } extracted(i)
    
        def extracted(i: Int): Unit = {
          println(i)
        }
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  def extractCase = {
    new FileSet {
      """
      object Demo {
        1 match {
	      /*(*/case _ => println(1)/*)*/
        }
      }
    """ becomes """
      object Demo {
        extracted
    
    	def extracted(): Unit = {
    	  1 match {
	        /*(*/case _ => println(1)/*)*/
          }
        }
      }
    """
    }.performRefactoring(extract("extracted", 0)).assertEqualTree
  }

  @Test
  def extractConstructorCall = new FileSet {
    """
      object Demo {
        /*(*/List(1, 2, 3)/*)*/
      }
    """ becomes
      """
      object Demo {
        extracted
    
    	val extracted = List(1, 2, 3)
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

  @Test
  @Ignore
  def extractImportedValue = new FileSet {
    """
      object Demo {
        def fn = {
          import scala.math.Pi
	  	  /*(*/Pi/*)*/
	    }
      }
    """ becomes
      """
      object Demo {
        def fn = {
          import scala.math.Pi
    	  extracted(Pi)
        }
    
        def extracted(Pi: Double) =
          Pi
      }
    """
  }.performRefactoring(extract("extracted", 2)).assertEqualTree
}