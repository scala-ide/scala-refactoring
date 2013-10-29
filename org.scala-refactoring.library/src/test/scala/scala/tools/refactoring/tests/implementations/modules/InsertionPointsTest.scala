package scala.tools.refactoring.tests.implementations.modules

import scala.tools.refactoring.tests.util.TestRefactoring
import org.junit.Assert._
import scala.tools.refactoring.implementations.modules.InsertionPoints

class InsertionPointsTest extends TestModule {
  abstract class InsertionTest(scopeIdx: Int, fs: FileSet) extends ModuleTest(fs){ self: InsertionPoints.InsertionPoint =>
	  import global._
	  
      def perform = {
        val t = insertInScope(scopes(scopeIdx), Literal(Constant(123)))
        transformFile(selection.file, t)
      }
  }
  
  def insertAtBeginningOfScope(scopeIdx: Int)(fs: FileSet) =
    (new InsertionTest(scopeIdx, fs) with InsertionPoints.AtBeginningOfAnyScope).perform
    
  def insertBeforeSelectionInScope(scopeIdx: Int)(fs: FileSet) =
    (new InsertionTest(scopeIdx, fs) with InsertionPoints.BeforeSelectionInAnyScope).perform
    
  def insertAfterSelectionInScope(scopeIdx: Int)(fs: FileSet) =
    (new InsertionTest(scopeIdx, fs) with InsertionPoints.AfterSelectionInAnyScope).perform
    
  def insertAfterSelectionInTemplate(scopeIdx: Int)(fs: FileSet) =
    (new InsertionTest(scopeIdx, fs) with InsertionPoints.AfterSelectionInTemplate).perform

  @Test
  def insertAtBeginningOfObject = new FileSet {
    """
    object Demo{
      val a = 1
      /*(*/val b = 2/*)*/
    }
    """ becomes
      """
    object Demo{
      123

      val a = 1
      /*(*/val b = 2/*)*/
    }
    """
  } applyRefactoring(insertAtBeginningOfScope(0))

  @Test
  def insertAtBeginningOfDef = new FileSet {
    """
    object Demo{
      def a = {
        val a = 1
        /*(*/val b = 2/*)*/
      }
    }
    """ becomes
      """
    object Demo{
      def a = {
        123
        val a = 1
        /*(*/val b = 2/*)*/
      }
    }
    """
  } applyRefactoring(insertAtBeginningOfScope(1))

  @Test
  def insertBeforeSelectionInObject = new FileSet {
    """
    object Demo{
      val a = 1
      /*(*/val b = 2/*)*/
    }
    """ becomes
      """
    object Demo{
      val a = 1

      123
      /*(*/val b = 2/*)*/
    }
    """
  } applyRefactoring(insertBeforeSelectionInScope(0))

  @Test
  def insertBeforeSelectionInDef = new FileSet {
    """
    object Demo{
      def a = {
        val a = 1
        /*(*/val b = 2/*)*/
      }
    }
    """ becomes
      """
    object Demo{
      def a = {
        val a = 1
        123
        /*(*/val b = 2/*)*/
      }
    }
    """
  } applyRefactoring(insertBeforeSelectionInScope(1))

  @Test
  def insertBeforeSelectionInOuterObject = new FileSet {
    """
    object Demo{
      def a = {
        val a = 1
        /*(*/val b = 2/*)*/
      }
    }
    """ becomes
      """
    object Demo{
      123

      def a = {
        val a = 1
        /*(*/val b = 2/*)*/
      }
    }
    """
  } applyRefactoring(insertBeforeSelectionInScope(0))

  @Test
  def insertBeforeSelectionInOuterDef = new FileSet {
    """
    object Demo{
      def a = {
        val aa = 1
        def b = {
          val a = 1
          /*(*/val b = 2/*)*/
        }
      }
    }
    """ becomes
      """
    object Demo{
      def a = {
        val aa = 1
        123
        def b = {
          val a = 1
          /*(*/val b = 2/*)*/
        }
      }
    }
    """
  } applyRefactoring(insertBeforeSelectionInScope(1))

  @Test
  def insertBeforeSelectionInSingleChildDef = new FileSet {
    """
    object Demo{
      def a =
        /*(*/"abc"/*)*/
    }
    """ becomes
      """
    object Demo{
      def a =
{
        123
        /*(*/"abc"
      }/*)*/
    }
    """
  } applyRefactoring(insertBeforeSelectionInScope(1))

  @Test
  def insertAfterSelectionInObject = new FileSet {
    """
    object Demo{
      val a = 1
      /*(*/val b = 2/*)*/
      val c = 3
    }
    """ becomes
      """
    object Demo{
      val a = 1
      /*(*/val b = 2/*)*/

      123
      val c = 3
    }
    """
  } applyRefactoring(insertAfterSelectionInScope(0))

  @Test
  def insertAfterSelectionInDef = new FileSet {
    """
    object Demo{
      def a = {
        val a = 1
        /*(*/val b = 2/*)*/
        val c = 3
      }
    }
    """ becomes
      """
    object Demo{
      def a = {
        val a = 1
        /*(*/val b = 2/*)*/
        123
        val c = 3
      }
    }
    """
  } applyRefactoring(insertAfterSelectionInScope(1))

  @Test
  def insertAfterSelectionInOuterObject = new FileSet {
    """
    object Demo{
      def a = {
        val a = 1
        /*(*/val b = 2/*)*/
      }

      val c = 3
    }
    """ becomes
      """
    object Demo{
      def a = {
        val a = 1
        /*(*/val b = 2/*)*/
      }

      123

      val c = 3
    }
    """
  } applyRefactoring(insertAfterSelectionInScope(0))

  @Test
  def insertAfterSelectionInOuterDef = new FileSet {
    """
    object Demo{
      def a = {
        val aa = 1
        def b = {
          val a = 1
          /*(*/val b = 2/*)*/
        }
        val c = 3
      }
    }
    """ becomes
      """
    object Demo{
      def a = {
        val aa = 1
        def b = {
          val a = 1
          /*(*/val b = 2/*)*/
        }
        123
        val c = 3
      }
    }
    """
  } applyRefactoring(insertAfterSelectionInScope(1))

  @Test
  def insertAfterSelectionInSingleChildDef = new FileSet {
    """
    object Demo{
      def a =
        /*(*/"abc"/*)*/
    }
    """ becomes
      """
    object Demo{
      def a =
{
        /*(*/"abc"
        123
      }/*)*/
    }
    """
  } applyRefactoring(insertAfterSelectionInScope(1))
  
  @Test
  def insertAfterSelectionInTemplates = new FileSet {
    """
    object Demo{
      val a = 1
      def aa = {
        /*(*/val b = 2/*)*/
      }
      val c = 3
    }
    """ becomes
      """
    object Demo{
      val a = 1
      def aa = {
        /*(*/val b = 2/*)*/
      }

      123
      val c = 3
    }
    """
  } applyRefactoring(insertAfterSelectionInTemplate(0))
  
  @Test
  def insertAfterSelectionInNestedTemplates = new FileSet {
    """
    object Demo{
      object A{
        object B{
          /*(*/println(1)/*)*/
        }
      }
    }
    """ becomes
      """
    object Demo{
      object A{
        object B{
          /*(*/println(1)/*)*/
        }

        123
      }
    }
    """
  } applyRefactoring(insertAfterSelectionInTemplate(1))
}