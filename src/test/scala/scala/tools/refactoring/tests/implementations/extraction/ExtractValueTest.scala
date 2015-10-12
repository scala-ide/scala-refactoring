package scala.tools.refactoring
package tests.implementations.extraction

import implementations.extraction.ExtractValue
import tests.util.TestHelper
import tests.util.TestRefactoring

class ExtractValueTest extends TestHelper with TestRefactoring {
  outer =>

  def extract(extractionIdx: Int)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractValue with TestProjectIndex
      val extraction = preparationResult.right.get.extractions(extractionIdx)
    }
    testRefactoring.performRefactoring(testRefactoring.extraction)
  }

  @Test
  def extractSimpleValue() = new FileSet {
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
          val extracted = /*(*/a * b
          println(extracted/*)*/)
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractSimpleSequence() = new FileSet {
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
          val extracted = {
            /*(*/println(a)
            println(b)/*)*/
          }
          extracted
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractWithOutboundDependency() = new FileSet {
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
          val extracted = {
                 /*(*/val b = a + 1/*)*/
                 b
               }
          /*(*/val b = extracted/*)*/
          println(b)
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractWithOutboundDependencies() = new FileSet {
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
          val extracted = {
                 /*(*/val b = a + 1
                 val c = a + 2
                 println(b*c)
                 val d = a + 3/*)*/
                 (c, d)
               }
          /*(*/val (c, d) = extracted
          println(c * d)
        }
      }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractToTemplateScope() = new FileSet {
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
          val b = extracted/*)*/
        }

        val extracted = /*(*/1 + 2

        def fm = 7
      }
    """
  }.performRefactoring(extract(2)).assertEqualTree

  @Test
  def extractImportedDependency() = new FileSet {
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
  }.performRefactoring(extract(1)).assertEqualTree

  @Test
  def extractImportedDependencyInSubtree() = new FileSet {
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
  }.performRefactoring(extract(1)).assertEqualTree

  @Test
  def extractImportedCtor() = new FileSet {
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
  }.performRefactoring(extract(1)).assertEqualTree

  @Test
  def extractImportedCtorWithImportedQualifiers() = new FileSet {
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
  }.performRefactoring(extract(1)).assertEqualSource

  @Test
  @Ignore
  def extractInFunctionWithSingleExprBody() = new FileSet{
    """
    object Demo {
      List(1, 2, 3).map(i => /*(*/i + 1/*)*/)
    }
    """ becomes """
    object Demo {
      List(1, 2, 3).map(i => {
        val extracted = i + 1
        extracted
      })
    }
    """
  }.performRefactoring(extract(0)).assertEqualSource

  @Test
  def extractFunction() = new FileSet{
    """
    object Demo {
      List(1, 2, 3).map(i /*(*/=> i + /*)*/1)
    }
    """ becomes """
    object Demo {
      List(1, 2, 3).map(extracted)

      val extracted: Int => Int = i => i + 1
    }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractFunctionWithBlockBody() = new FileSet{
    """
    object Demo {
      List(1, 2, 3).map{/*(*/i =>
      val a = 1
      i + a/*)*/
    }
    }
    """ becomes """
    object Demo {
      List(1, 2, 3).map(extracted)

      val extracted: Int => Int = i => {
      val a = 1
      i + a
      }
    }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractFunctionWithWildcardParam() = new FileSet{
    """
    object Demo {
      List(1, 2, 3).map(/*(*/_ + 1/*)*/)
    }
    """ becomes """
    object Demo {
      List(1, 2, 3).map(extracted)

      val extracted: Int => Int = _ + 1
    }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractConstantPattern() = new FileSet{
    """
    object Demo {
    List(1) match {
      case List(/*(*/1/*)*/) => ()
      }
    }
    """ becomes """
    object Demo {
    List(1) match {
      case List(extracted) => ()
      }

      val extracted = 1
    }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def dontExtractPatternsWithBindings() = new FileSet{
    """
    case class Intish(i: Int)
    object Demo {
    Intish(1) match {
      case /*(*/Intish(i)/*)*/ => i
      }
    }
    """ becomes """
    case class Intish(i: Int)
    object Demo {
      extracted

    val extracted = Intish(1) match {
      case Intish(i) => i
      }
    }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def dontExtractWildcardPatterns() = new FileSet(expectCompilingCode = false) {
    """
    case class Intish(i: Int)
    object Demo {
    Intish(1) match {
      case /*(*/_/*)*/ => i
      }
    }
    """ becomes """
    case class Intish(i: Int)
    object Demo {
      extracted

    val extracted = Intish(1) match {
      case _ => i
      }
    }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractIntoNewValBlock() = new FileSet{
    """
    object Demo {
    val a = /*(*/"hello"/*)*/.toUpperCase
    }
    """ becomes """
    object Demo {
    val a = {
        val extracted = "hello"
        extracted.toUpperCase
      }
    }
    """
  }.performRefactoring(extract(0)).assertEqualTree

  @Test
  def extractMutable() = new FileSet{
    """
    object Demo {
    def fn = {
      /*(*/var i = 1/*)*/
      i += 1
    }
    }
    """ becomes """
    object Demo {
    def fn = {
      val extracted = {
        var i = 1
          i
        }
        var i = extracted
        i += 1
      }
    }
    """
  }.performRefactoring(extract(0)).assertEqualTree
}
