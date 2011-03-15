/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import org.junit.Assert._
import tests.util.{TestHelper, TestRefactoring}
import scala.tools.refactoring.implementations.EliminateMatch

class EliminateMatchTest extends TestHelper with TestRefactoring {
  outer =>
  
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
    
    def f2(x: Option[String]) = x isDefined
    
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
  
  def elim(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new EliminateMatch with SilentTracing {
      val global = outer.global
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters)
  }.changes
  
  @Test
  def existsElimination = new FileSet {
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/ match /*)*/ {
          case Some(s) => s == "s"
          case None => false
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/exists (s => s == "s")
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
          case Some(_) => /*yes it's*/ true
        }
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/exists (_ => /*yes it's*/ true)
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
      }
    """ becomes
    """
      package elimination
      object EliminateMap {
        (Some("s"): Option[String]) /*(*/exists (s =>
            val x = s.toInt
            if(x % 2 == 0)
              false
            else
              true)
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
}
