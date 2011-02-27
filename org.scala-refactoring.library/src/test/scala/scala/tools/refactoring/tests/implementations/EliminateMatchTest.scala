/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import org.junit.Assert._
import tests.util.{TestHelper, TestRefactoring}

class EliminateMatchTest extends TestHelper with TestRefactoring {
  
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
}
