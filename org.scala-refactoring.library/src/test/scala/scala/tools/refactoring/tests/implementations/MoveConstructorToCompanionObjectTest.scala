package scala.tools.refactoring
package tests.implementations

import implementations.MoveConstructorToCompanionObject
import tests.util.TestHelper
import tests.util.TestRefactoring

class MoveConstructorToCompanionObjectTest extends TestHelper with TestRefactoring {

  outer =>
    
  def moveConstructorToCompanion(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new MoveConstructorToCompanionObject with SilentTracing with GlobalIndexes {
      val global = outer.global
      val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
      val index = GlobalIndex(cuIndexes)
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters)
  }.changes

  
  @Test
  def moveConstructorToExistingCompanion = new FileSet {
    """
      package moveConstructorToCompanion.existingCompanion

      object Foo /* FIXME () */ {

      }

      class /*(*/Foo/*)*/(val p: Int)
    """ becomes
    """
      package moveConstructorToCompanion.existingCompanion

      object Foo /* FIXME () */ () {
        def apply(p: Int) = {
          new Foo(p)
        }
      }

      class /*(*/Foo/*)*/(val p: Int)
    """
  } applyRefactoring(moveConstructorToCompanion)
  
  @Test
  def existingCompanionWithMethods = new FileSet {
    """
      package moveConstructorToCompanion.existingCompanionWithMethods

      object Foo /* FIXME () */ {
        def bar(a: Int) = {
          a * 57
        }
      }

      class /*(*/Foo/*)*/(val p: Int)
    """ becomes
    """
      package moveConstructorToCompanion.existingCompanionWithMethods

      object Foo /* FIXME () */ () {
        def apply(p: Int) = {
          new Foo(p)
        }
        def bar(a: Int) = {
          a * 57
        }
      }

      class /*(*/Foo/*)*/(val p: Int)
    """
  } applyRefactoring(moveConstructorToCompanion)
  
  @Test
  def withoutExistingCompanion = new FileSet {
    """
      package moveConstructorToCompanion.withoutExistingCompanion

      class /*(*/Foo/*)*/(val p: Int)
    """ becomes
    """
      package moveConstructorToCompanion.withoutExistingCompanion
      object Foo {
        def apply(p: Int) = {
          new Foo(p)
        }
      }

      class /*(*/Foo/*)*/(val p: Int)
    """
  } applyRefactoring(moveConstructorToCompanion)
  
}