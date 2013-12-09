package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractValue
import tests.util.TestHelper
import tests.util.TestRefactoring

class ExtractValueTest extends TestHelper with TestRefactoring {
  outer =>

  def extract(name: String, extractionIdx: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractValue with SilentTracing with TestProjectIndex
      val extraction = preparationResult.right.get.extractions(extractionIdx)
        .withAbstractionName(name)
    }
    testRefactoring.performRefactoring(testRefactoring.extraction)
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
  }.performRefactoring(extract("c", 0)).assertEqualTree

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
  }.performRefactoring(extract("c", 0)).assertEqualTree

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
  }.performRefactoring(extract("c", 0)).assertEqualTree

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
  }.performRefactoring(extract("e", 0)).assertEqualTree

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
        def fn(a: Int) = {
          val b = c/*)*/
        }

        val c = /*(*/1 + 2

        def fm = 7
      }
    """
  }.performRefactoring(extract("c", 1)).assertEqualTree

  @Test
  def extractImportedDependency = new FileSet {
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
	  	  extracted
        }
    
    	val extracted = {
    	  import scala.math.Pi
          Pi
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree

  @Test
  def extractImportedDependencyInSubtree = new FileSet {
    """
      object Demo {
        def fn = {
          import scala.math.Pi
	  	  /*(*/100 * Pi/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        def fn = {
          import scala.math.Pi
	  	  extracted
        }
    
    	val extracted = {
          import scala.math.Pi
          100 * Pi
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree

  @Test
  def extractImportedCtor = new FileSet {
    """
      object Demo {
        def fn = {
          import scala.collection.mutable.LinkedList
	  	  /*(*/LinkedList(1)/*)*/
        }
      }
    """ becomes
      """
      object Demo {
        def fn = {
          import scala.collection.mutable.LinkedList
    	  extracted
        }
    
    	val extracted = {
          import scala.collection.mutable.LinkedList
          scala.collection.mutable.LinkedList(1)
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree

  @Test
  def extractImportedCtorWithImportedQualifiers = new FileSet {
    """
      import scala.collection.mutable.LinkedList
      object Demo {
        def fn = {
	  	  /*(*/LinkedList(1)/*)*/
        }
      }
    """ becomes
      """
      import scala.collection.mutable.LinkedList
      object Demo {
        def fn = extracted

        val extracted = {
	  	  /*(*/LinkedList(1)/*)*/
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualSource
}