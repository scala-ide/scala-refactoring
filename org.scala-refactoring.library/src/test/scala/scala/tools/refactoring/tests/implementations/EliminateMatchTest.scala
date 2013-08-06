/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import org.junit.Assert._
import tests.util.TestHelper
import tests.util.TestRefactoring
import scala.tools.refactoring.implementations.EliminateMatch

import language.reflectiveCalls

class EliminateMatchTest extends TestHelper with TestRefactoring {
  outer =>

  def elim(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new EliminateMatch with SilentTracing with common.InteractiveScalaCompiler {
      val global = outer.global
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters)
  }.changes

  val some = Some("s"): Option[String]
  val none = None: Option[String]

  @Test
  def eliminateOptionMap {

    def f1(x: Option[String]) = x match {
      case Some(s) => Some(s * 2)
      case None => None
    }

    def f2(x: Option[String]) = x map (s => s * 2)

    assertEquals(f1(some), f2(some))
    assertEquals(f1(none), f2(none))
  }

  @Test
  def eliminateOptionExists {

    def f1(x: Option[String]) = x match {
      case Some(s) => s contains "a"
      case None => false
    }

    def f2(x: Option[String]) = x exists (s => s contains "a")

    assertEquals(f1(some), f2(some))
    assertEquals(f1(none), f2(none))
  }

  @Test
  def eliminateOptionIsDefined {

    def f1(x: Option[String]) = x match {
      case Some(_) => true
      case None => false
    }

    def f2(x: Option[String]) = x.isDefined

    assertEquals(f1(some), f2(some))
    assertEquals(f1(none), f2(none))
  }

  @Test
  def eliminateOptionForeach {

    var x1 = 0
    var x2 = 0

    def f1(x: Option[String]) = x match {
      case Some(s) => x1 += 1
      case None =>
    }

    def f2(x: Option[String]) = x foreach (s => x2 += 1)

    assertEquals(f1(some), f2(some))
    assertEquals(f1(none), f2(none))
    assertEquals(1, x1)
    assertEquals(1, x2)
  }

  @Test
  def eliminateIsDefined = new FileSet {
    """
    object EliminiateWithoutSelection {
      def f1(x: Option[String]) = x /*(*/ match /*)*/ {
        case Some(_) => true
        case None => false
      }
    }
    """ becomes
    """
    object EliminiateWithoutSelection {
      def f1(x: Option[String]) = x /*(*/isDefined
    }
    """
  } applyRefactoring(elim)

  @Test
  def flatMapElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        val x: Option[String] = Some("some")
        (Some("s"): Option[String]) /*(*/ match /*)*/ {
          case None => None
          case Some(s) => x
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        val x: Option[String] = Some("some")
        (Some("s"): Option[String]) /*(*/flatMap (s => x)
      }
    """
  } applyRefactoring(elim)

  @Test
  def flatMapElimination2 = new FileSet {
    """
      package elimination
      object EliminateMap {
        def cont(s: String) = Some(s*2)
        (Some("s"): Option[String]) /*(*/ match /*)*/ {
          case Some(s) => cont(s)
          case _ => None
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def cont(s: String) = Some(s*2)
        (Some("s"): Option[String]) /*(*/flatMap (s => cont(s))
      }
    """
  } applyRefactoring(elim)

  @Test
  def simpleExistsElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/ match /*)*/ {
          case Some(s) => true || false
          case None => false
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/exists (s => true || false)
      }
    """
  } applyRefactoring(elim)

  @Test
  def isDefinedElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/ match /*)*/ {
          case Some(s) => true
          case None => false
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/isDefined
      }
    """
  } applyRefactoring(elim)

  @Test
  def existsEliminationDifferentOrder = new FileSet {
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/ match /*)*/ {
          case None => false
          case Some(_) => /*yes it's*/ 1 + 0 == 1
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/exists (_ => /*yes it's*/ 1 + 0 == 1)
      }
    """
  } applyRefactoring(elim)

  @Test // TODO: could be more beautiful
  def existsEliminationWithBody = new FileSet {
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/ match /*)*/ {
          case Some(s) =>
            val x = s.toInt
            if(x % 2 == 0)
              false
            else
              true
          case None => false
        }
        // and:
        val x = 5
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/exists ({s =>
            val x = s.toInt
            if(x % 2 == 0)
              false
            else
              true
})
        // and:
        val x = 5
      }
    """
  } applyRefactoring(elim)

  @Test
  def mapEliminationSimple = new FileSet {
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/ match /*)*/ {
          case Some(s) => Some(s * 2)
          case None => None
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/map (s => s * 2)
      }
    """
  } applyRefactoring(elim)

  @Test
  def mapEliminationNoneSome = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/ match /*)*/ {
          case None => None
          case Some(s) => Some(s * 2)
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/map (s => s * 2)
      }
    """
  } applyRefactoring(elim)

  @Test
  def mapEliminationNoCapture = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/ match /*)*/ {
          case None => None
          case Some(_) => Some(true)
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/map (_ => true)
      }
    """
  } applyRefactoring(elim)

  @Test
  def mapEliminationNoNone = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/ match /*)*/ {
          case Some(x) => Some(x)
          case _ => None
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/map (x => x)
      }
    """
  } applyRefactoring(elim)

  @Test
  def mapEliminationWithChainedCall = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = (x /*(*/ match /*)*/ {
          case Some(x) => Some(x)
          case _ => None
        }) isDefined
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = (x /*(*/map (x => x)) isDefined
      }
    """
  } applyRefactoring(elim)

  @Test
  def mapEliminationWithBlock = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case Some(s) =>
              val x = 5
              Some(s * x)
            case None =>
              None
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/map (s => {
          val x = 5
          s * x
        })
      }
    """
  } applyRefactoring(elim)

  @Test
  def foreachEliminationWithExplicitUnit = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case Some(s) => println(s)
            case None => ()
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/foreach (s => println(s))
      }
    """
  } applyRefactoring(elim)

  @Test
  def foreachEliminationWithUnit = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case Some(s) => println(s)
            case None =>
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/foreach (s => println(s))
      }
    """
  } applyRefactoring(elim)

  @Test
  def flattenElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case Some(s) => s
            case None => None
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/flatten
      }
    """
  } applyRefactoring(elim)

  @Test
  def isEmptyElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case Some(s) => false
            case None => true
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/isEmpty
      }
    """
  } applyRefactoring(elim)

  @Test
  def forallElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case Some(s) => s == "something"
            case None => true
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/forall (s => s == "something")
      }
    """
  } applyRefactoring(elim)

  @Test
  def orElseElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case None => Some("the else")
            case Some(s) => Some(s)
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/orElse (Some("the else"))
      }
    """
  } applyRefactoring(elim)

  @Test
  def orElseEliminationAlt = new FileSet {
    """
      package elimination
      object EliminateMap {
        val els: Option[String] = Some("...")
        def m(x: Option[String]) = {
          x /*(*/ match /*)*/ {
            case None => els
            case Some(x) => Some(x)
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        val els: Option[String] = Some("...")
        def m(x: Option[String]) = x /*(*/orElse (els)
      }
    """
  } applyRefactoring(elim)

  @Test
  def getOrElseElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case None => 42
            case Some(x) => x
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/getOrElse (42)
      }
    """
  } applyRefactoring(elim)

  @Test
  def toListElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case None => Nil
            case Some(x) => x :: Nil
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/toList
      }
    """
  } applyRefactoring(elim)

  @Test
  def toListEliminationAlt = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case None => Nil
            case Some(x) => List(x)
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = x /*(*/toList
      }
    """
  } applyRefactoring(elim)

  @Test
  def foreachInBlock = new FileSet {
    """
      package elimination
      object EliminateMap {
        def someMethod(x: Int) = {
          val comp = {
            if(x == 5) {
              "5"
            } else {
              "3"
            }
          }

          val somethingElse = Option(42)
          // a comment
          somethingElse /*(*/ match /*)*/ {
            case Some(x) => println(x)
            case None =>
          }
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        def someMethod(x: Int) = {
          val comp = {
            if(x == 5) {
              "5"
            } else {
              "3"
            }
          }

          val somethingElse = Option(42)
          // a comment
          somethingElse /*(*/foreach (x => println(x))
        }
      }
    """
  } applyRefactoring(elim)

  @Test(expected=classOf[PreparationException])
  def cannotEliminateWithBinding = new FileSet {
    """
      package elimination
      object EliminateMap {
        def m(x: Option[Int]) = {
          x /*(*/ match /*)*/ {
            case None => Nil
            case Some(x: Int) => List(x)
          }
        }
      }
    """ becomes
    ""
  } applyRefactoring(elim)
}
