package scala.tools.refactoring.tests.implementations.modules

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.implementations.modules.ExtractionSources
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.util.CompilerProvider
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.Refactoring
import org.junit.Assert._

class ExtractionSourcesTest extends TestModule { outer =>
  import global._
  val t123 = Literal(Constant(123))

  def refactoring(fs: FileSet) = new ModuleTest(fs) with ExtractionSources.FromExpressions {
    def replace(replacement: Tree) = {
      val t = replaceBy(replacement.asInstanceOf[this.global.Tree])
      transformFile(selection.file, t)
    }
  }

  def replaceExpressions(replacement: Tree)(fs: FileSet) = {
    refactoring(fs).replace(replacement)
  }
  
  def assertPreparationFailure(file: String) = {
    val fs = new FileSet{
      file becomes ""
    }
    assertTrue(refactoring(fs).preparationError.isDefined)
  }

  @Test
  def replaceSingleExpression = new FileSet {
    """
    object Demo {
      println(0)
      /*(*/println(1)/*)*/
      println(2)
    }
    """ becomes
      """
    object Demo {
      println(0)
      /*(*/123/*)*/
      println(2)
    }
    """
  } applyRefactoring (replaceExpressions(t123))

  @Test
  def replaceMultipleExpressions = new FileSet {
    """
    object Demo {
      println(0)
      /*(*/println(1)
      println(2)
      println(3)/*)*/
      println(4)
    }
    """ becomes
      """
    object Demo {
      println(0)

      123
      println(4)
    }
    """
  } applyRefactoring (replaceExpressions(t123))
  
  @Test
  def replaceNothing = assertPreparationFailure("""
    object Demo {
      println(0)
    }
  """)

  @Test
  @Ignore
  def expandSelectionToWholeTrees = new FileSet {
    """
    object Demo {
      /*(*/println(1)
      {
        println(2)/*)*/
        println(3)
      }
    }
    """ becomes
      """
    object Demo {
      /*(*/123/*)*/
    }
    """
  } applyRefactoring (replaceExpressions(t123))
}