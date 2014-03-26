package scala.tools.refactoring
package tests.implementations

import implementations.MoveConstructorToCompanionObject
import tests.util.TestHelper
import tests.util.TestRefactoring

import language.reflectiveCalls

class MoveConstructorToCompanionObjectTest extends TestHelper with TestRefactoring {

  def moveConstructorToCompanion(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new MoveConstructorToCompanionObject with SilentTracing with TestProjectIndex
    val changes = performRefactoring()
  }.changes

  @Test
  def moveConstructorToExistingCompanion() = new FileSet {
    """
      package moveConstructorToCompanion.existingCompanion

      object Foo {

      }

      class /*(*/Foo/*)*/(val p: Int)
    """ becomes
    """
      package moveConstructorToCompanion.existingCompanion

      object Foo {
        def apply(p: Int) = {
          new Foo(p)
        }
      }

      class /*(*/Foo/*)*/(val p: Int)
    """
  } applyRefactoring(moveConstructorToCompanion)

  @Test
  def existingCompanionWithMethods() = new FileSet {
    """
      package moveConstructorToCompanion.existingCompanionWithMethods

      object Foo {
        def bar(a: Int) = {
          a * 57
        }
      }

      class /*(*/Foo/*)*/(val p: Int)
    """ becomes
    """
      package moveConstructorToCompanion.existingCompanionWithMethods

      object Foo {
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
  def withoutExistingCompanion() = new FileSet {
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

  @Test
  def withoutExistingCompanionWithEnclosingClass() = new FileSet {
    """
      package moveConstructorToCompanion.withoutExistingCompanionWithEnclosingClass

      class Enclosing {
        class /*(*/Inner/*)*/(val p: Int) {

        }
      }
    """ becomes
    """
      package moveConstructorToCompanion.withoutExistingCompanionWithEnclosingClass

      class Enclosing {
        object Inner {
          def apply(p: Int) = {
            new Inner(p)
          }
        }

        class /*(*/Inner/*)*/(val p: Int) {

        }
      }
    """
  } applyRefactoring(moveConstructorToCompanion)

  @Test
  def withoutExistingCompanionWithEnclosingObject() = new FileSet {
    """
      package moveConstructorToCompanion.withoutExistingCompanionWithEnclosingObject

      object Enclosing {
        class /*(*/Inner/*)*/(val p: Int) {

        }
      }
    """ becomes
    """
      package moveConstructorToCompanion.withoutExistingCompanionWithEnclosingObject

      object Enclosing {
        object Inner {
          def apply(p: Int) = {
            new Inner(p)
          }
        }

        class /*(*/Inner/*)*/(val p: Int) {

        }
      }
    """
  } applyRefactoring(moveConstructorToCompanion)

  @Test
  def withoutExistingCompanionWithEnclosingMethod() = new FileSet {
    """
      package moveConstructorToCompanion.withoutExistingCompanionWithEnclosingMethod

      class Foo {
        def enclosing(a: String) = {
          class /*(*/Inner/*)*/(p: Int)
          "foo"
        }
      }
    """ becomes
    """
      package moveConstructorToCompanion.withoutExistingCompanionWithEnclosingMethod

      class Foo {
        def enclosing(a: String) = {
          object Inner {
            def apply(p: Int) = {
              new Inner(p)
            }
          }
          class /*(*/Inner/*)*/(p: Int)
          "foo"
        }
      }
    """
  } applyRefactoring(moveConstructorToCompanion)

  @Test
  def withoutExistingCompanionWithEnclosingVal() = new FileSet {
    """
      package moveConstructorToCompanion.withoutExistingCompanionWithEnclosingVal

      class Foo {
        val enclosing = {
          class /*(*/Inner/*)*/(p: Int)
          "foo"
        }
      }
    """ becomes
    """
      package moveConstructorToCompanion.withoutExistingCompanionWithEnclosingVal

      class Foo {
        val enclosing = {
          object Inner {
            def apply(p: Int) = {
              new Inner(p)
            }
          }
          class /*(*/Inner/*)*/(p: Int)
          "foo"
        }
      }
    """
  } applyRefactoring(moveConstructorToCompanion)

  @Test
  def withTypeParams() = new FileSet {
    """
      package moveConstructorToCompanion.withTypeParams

      object Foo

      class /*(*/Foo/*)*/[A, B, C](a: A, b: B, c: C)
    """ becomes
    """
      package moveConstructorToCompanion.withTypeParams

      object Foo {
        def apply[A, B, C](a: A, b: B, c: C) = {
          new Foo(a, b, c)
        }
      }

      class /*(*/Foo/*)*/[A, B, C](a: A, b: B, c: C)
    """
  } applyRefactoring(moveConstructorToCompanion)

  @Test
  def curriedConstructor() = new FileSet {
    """
      package moveConstructorToCompanion.curriedConstructor

      object Foo

      class /*(*/Foo/*)*/[A, B, C](a: A)(b: B)(c: C)
    """ becomes
    """
      package moveConstructorToCompanion.curriedConstructor

      object Foo {
        def apply[A, B, C](a: A)(b: B)(c: C) = {
          new Foo(a)(b)(c)
        }
      }

      class /*(*/Foo/*)*/[A, B, C](a: A)(b: B)(c: C)
    """
  } applyRefactoring(moveConstructorToCompanion)

  @Test
  def constructorWithoutParameters() = new FileSet {
    """
      package moveConstructorToCompanion.emptyConstructor

      class /*(*/Foo/*)*/ {
      }
    """ becomes
    """
      package moveConstructorToCompanion.emptyConstructor

      object Foo {
        def apply() = {
          new Foo()
        }
      }

      class /*(*/Foo/*)*/ {
      }
    """
  } applyRefactoring(moveConstructorToCompanion)

  @Test
  def replaceConstructorCallsWithoutExistingCompanion() = new FileSet {
    """
      package moveConstructorToCompanion.replaceConstructorCallsWithoutExistingCompanion

      class /*(*/Foo/*)*/(val p: Int)

      class User {
        val foo = new Foo(57)
      }
    """ becomes
    """
      package moveConstructorToCompanion.replaceConstructorCallsWithoutExistingCompanion

      object Foo {
        def apply(p: Int) = {
          new Foo(p)
        }
      }

      class /*(*/Foo/*)*/(val p: Int)

      class User {
        val foo = Foo.apply(57)
      }
    """
  } applyRefactoring(moveConstructorToCompanion)


  @Test
  def replaceConstructorCallsInObject() = new FileSet {
    """
      package moveConstructorToCompanion.replaceConstructorCallsInObject

      class /*(*/Foo/*)*/(val p: Int)

      object User {
        val foo = new Foo(57)
      }
    """ becomes
    """
      package moveConstructorToCompanion.replaceConstructorCallsInObject

      object Foo {
        def apply(p: Int) = {
          new Foo(p)
        }
      }

      class /*(*/Foo/*)*/(val p: Int)

      object User {
        val foo = Foo.apply(57)
      }
    """
  } applyRefactoring(moveConstructorToCompanion)

}