package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractMethod
import tests.util.TestHelper
import tests.util.TestRefactoring

class ExtractMethodTest extends TestHelper with TestRefactoring {
  def extract(name: String, extractionIdx: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractMethod with SilentTracing with TestProjectIndex
      val e = preparationResult.right.get.extractions(extractionIdx).asInstanceOf[refactoring.MethodExtraction]
      val extraction = e.copy(abstractionName = name)
    }
    testRefactoring.performRefactoring(testRefactoring.extraction)
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
          def extracted() = {
            /*(*/a * b
          }
          println(/*(*/extracted()/*)*/)
        }
      }
    """
  }.performRefactoring(extract("extracted", 0)).assertEqualTree

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
            /*(*/val (b, c, d) = extracted(p, a)
            println(b * b * c * d * d)
          }
        }

        def extracted(p: Int, a: Int) = {
          /*(*/val b = p * a
          val c = a * na
          val d = fm(a)/*)*/
          (b, c, d)
        }

        def fm(p: Int) = p + 1
      }
    """
  }.performRefactoring(extract("extracted", 2)).assertEqualTree

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
	  	  extracted()
        }
    
    	def extracted() = {
    	  import scala.math.Pi
    	  Pi
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree
  
  @Test
  def extractFromNestedClass = new FileSet {
    """
      object Demo {
        trait T{
          val a = 1
          /*(*/a/*)*/
	    }
      }
    """ becomes
      """
      object Demo {
        trait T{
          val a = 1
          extracted(a)
	    }
    	def extracted(a: Int) = a
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree

  @Test
  @Ignore("Has to be handled in pretty printer")
  def hideImportedQualifiersOfParameter = new FileSet {
    """
      import scala.collection.mutable.LinkedList
      object Demo {
        def fn = {
          val l = new LinkedList[Int]
          /*(*/l.length/*)*/
        }
      }
    """ becomes
      """
      import scala.collection.mutable.LinkedList
      object Demo {
        def fn = {
          val l = new LinkedList[Int]
          extracted(l)
        }

        def extracted(l: LinkedList[Int]) = {
          /*(*/l.length/*)*/
        }
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualSource
  
  @Test(expected=classOf[IndexOutOfBoundsException])
  def extractWithSideEffects = new FileSet {
    """
      object Demo {
        def fn = {
          var a = 1
	  	  /*(*/a += 1/*)*/
          a
	    }
      }
    """ becomes
      """"""
  }.performRefactoring(extract("extracted", 1)).assertEqualSource
  
  @Test
  def extractFunction = new FileSet {
    """
      object Demo {
	  	def fn(a: Int) = {
          List(1, 2).map{/*(*/i => i + a/*)*/}
        }
      }
    """ becomes
      """
      object Demo {
	  	def fn(a: Int) = {
          List(1, 2).map{ extracted(a) }
        }
    	def extracted(a: Int): Int => Int =
    	  i => i + a
      }
    """
  }.performRefactoring(extract("extracted", 1)).assertEqualTree
}