/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import tests.util.TestHelper
import analysis._
import org.junit.Assert._

class NameValidationTest extends TestHelper with NameValidation with GlobalIndexes {

  var index: IndexLookup = null
  
  @Test
  def validIdentifiers() {
    assertTrue(isValidIdentifier("x"))
    assertTrue(isValidIdentifier("Object"))
    assertTrue(isValidIdentifier("maxIndex"))
    assertTrue(isValidIdentifier("p2p"))
    assertTrue(isValidIdentifier("empty_?"))
    assertTrue(isValidIdentifier("+"))
    assertTrue(isValidIdentifier("__system"))
    assertTrue(isValidIdentifier("`yield`"))
    assertTrue(isValidIdentifier("_MAX_LEN_"))
    assertTrue(isValidIdentifier("αρτη"))
    assertTrue(isValidIdentifier("_y"))
    assertTrue(isValidIdentifier("dot_product_*"))
  }
  
  @Test
  def invalidIdentifiers() {
    assertFalse(isValidIdentifier("a b"))
    assertFalse(isValidIdentifier("-abcde-"))
    assertFalse(isValidIdentifier("def"))
    assertFalse(isValidIdentifier("_"))
    assertFalse(isValidIdentifier("`"))
  }
  
  implicit def treeFinder(t: global.Tree) = new {
    def find(name: String) = {
      TreeSelection(t).allSelectedTrees find {
        case t: global.SymTree => t.symbol.nameString == name
        case _ => false
      }
    }
  }
  
  def checkNameForCollision(s: String, t: global.Tree, r: global.Tree) = 
    !doesNameCollide(s, t.symbol, GlobalIndex(List(CompilationUnitIndex(r)))).isEmpty

  @Test
  def localNameCollision() {
    val tree = treeFrom("""
    class A {
      def method1 {
        val a = 5
        val b = 5
      }
      def method2 {
        val x = 5
      }
    }
    """)
    
    val valA = tree.find("a").get
    
    val checkName = checkNameForCollision(_: String, valA, tree)
    
    assertFalse(checkName("c"))
    assertFalse(checkName("x"))
    assertFalse(checkName("A"))
    assertFalse(checkName("method2"))

    assertTrue (checkName("b"))
    assertTrue (checkName("a"))
    assertTrue (checkName("method1"))
  }
  
  @Test
  def methodNameCollision() {
    val tree = treeFrom("""
    class A {
      def method1 {
      }
      def method2 {
      }
    }

    class B extends A {
      def method3 {
      }
    }

    class C {
      def method1 {
      }
      def method2 {
      }
      def method4 {
      }
    }

    class D(val aString: String) extends A
    """)
    
    val valA = tree.find("method1").get
    
    val checkName = checkNameForCollision(_: String, valA, tree)
    
    assertFalse(checkName("blabla"))
    assertFalse(checkName("method4"))
    assertFalse(checkName("A"))
    
    assertTrue(checkName("method2"))
    assertTrue(checkName("method3"))
    assertTrue(checkName("aString"))
  }
  
  @Test
  def collisionInPackage() {
    val tree = treeFrom("""
    package justapackage1
    
    class C1
   
    package p2 {
      class A
      class C4
      package p21 { class C2 { class Ok1 } }
      package p21 { class C3 }
      package p22 { class C3 }
    }

    package p3 {
      class Ok2
    }
    """)
    
    val valA = tree.find("A").get
    
    val checkName = checkNameForCollision(_: String, valA, tree) 
    
    assertFalse(checkName("Ok1"))
    assertFalse(checkName("Ok2"))
    assertFalse(checkName("B"))

    assertTrue(checkName("C1"))
    assertTrue(checkName("C2"))
    assertTrue(checkName("C3"))
    assertTrue(checkName("C4"))
  }
}

