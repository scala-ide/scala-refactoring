/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import tests.util.TestRefactoring
import tests.util.TestHelper
import scala.tools.refactoring.implementations.ExpandCaseClassBinding

class ExpandCaseClassBindingTest extends TestHelper with TestRefactoring {

  def expand(pro: FileSet) = new TestRefactoringImpl(pro) {
    override val refactoring = new ExpandCaseClassBinding with TestProjectIndex
    val changes = performRefactoring()
  }.changes

  @Test
  @ScalaVersion(doesNotMatch = "2.12")
  def expandSome() = new FileSet {
    """
      package extractLocal
      object Demo {
        Some("string") match {
          case /*(*/s/*)*/ =>
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        Some("string") match {
          case /*(*/Some(x)/*)*/ =>
        }
      }
    """
  } applyRefactoring(expand)

  @Test
  @ScalaVersion(matches = "2.12")
  def expandSome_2_12() = new FileSet {
    """
      package extractLocal
      object Demo {
        Some("string") match {
          case /*(*/s/*)*/ =>
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        Some("string") match {
          case /*(*/Some(value)/*)*/ =>
        }
      }
    """
  } applyRefactoring(expand)

  @Test
  @ScalaVersion(doesNotMatch = "2.12")
  def expandSomeWithReference() = new FileSet {
    """
      package extractLocal
      object Demo {
        Some("string") match {
          case /*(*/s/*)*/ => s
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        Some("string") match {
          case /*(*/s @ Some(x)/*)*/ => s
        }
      }
    """
  } applyRefactoring(expand)

  @Test
  @ScalaVersion(matches = "2.12")
  def expandSomeWithReference_2_12() = new FileSet {
    """
      package extractLocal
      object Demo {
        Some("string") match {
          case /*(*/s/*)*/ => s
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        Some("string") match {
          case /*(*/s @ Some(value)/*)*/ => s
        }
      }
    """
  } applyRefactoring(expand)

  @Test
  def expandWithMultipleParams() = new FileSet {
    """
      package expandWithMultipleParams
      case class Abc(a: String, b: Int, c: Double)
      object Demo {
        Abc("", 5, 5.0) match {
          case /*(*/whatever/*)*/ =>
        }
      }
    """ becomes
    """
      package expandWithMultipleParams
      case class Abc(a: String, b: Int, c: Double)
      object Demo {
        Abc("", 5, 5.0) match {
          case /*(*/Abc(a, b, c)/*)*/ =>
        }
      }
    """
  } applyRefactoring(expand)

  @Test
  def expandTuple() = new FileSet {
    """
      object Demo {
        ("", 5, 5.0) match {
          case /*(*/whatever/*)*/ =>
        }
      }
    """ becomes
    """
      object Demo {
        ("", 5, 5.0) match {
          case /*(*/(_1, _2, _3)/*)*/ =>
        }
      }
    """
  } applyRefactoring(expand)

  @Test
  def expandNested() = new FileSet {
    """
      case class Pair[T, U](first: T, second: U)
      object Demo {
        List(Pair(true, false)) match {
          case /*(*/whatever/*)*/ :: Nil => whatever
        }
      }
    """ becomes
    """
      case class Pair[T, U](first: T, second: U)
      object Demo {
        List(Pair(true, false)) match {
          case /*(*/whatever @ Pair(first, second)/*)*/ :: Nil => whatever
        }
      }
    """
  } applyRefactoring(expand)

  @Test
  @ScalaVersion(doesNotMatch = "2.12")
  def expandInnerNested() = new FileSet {
    """
      case class Pair[T, U](first: T, second: U)
      object Demo {
        List(Pair(Some(true), false)) match {
          case pair @ Pair(/*(*/first/*)*/, second) :: Nil =>
        }
      }
    """ becomes
    """
      case class Pair[T, U](first: T, second: U)
      object Demo {
        List(Pair(Some(true), false)) match {
          case pair @ Pair(/*(*/Some(x)/*)*/, second) :: Nil =>
        }
      }
    """
  } applyRefactoring(expand)

  @Test
  @ScalaVersion(matches = "2.12")
  def expandInnerNested_2_12() = new FileSet {
    """
      case class Pair[T, U](first: T, second: U)
      object Demo {
        List(Pair(Some(true), false)) match {
          case pair @ Pair(/*(*/first/*)*/, second) :: Nil =>
        }
      }
    """ becomes
    """
      case class Pair[T, U](first: T, second: U)
      object Demo {
        List(Pair(Some(true), false)) match {
          case pair @ Pair(/*(*/Some(value)/*)*/, second) :: Nil =>
        }
      }
    """
  } applyRefactoring(expand)

  @Test(expected=classOf[PreparationException])
  def illegalExpansion() = new FileSet {
    """
      package illegalExpansion
      object /*(*/Xy/*)*/ {

      }
    """ becomes
    """"""
  } applyRefactoring(expand)
}
