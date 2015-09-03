/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import tests.util.TestHelper
import analysis._
import org.junit.Assert._

import language.{reflectiveCalls, implicitConversions}

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

  def nameAlreadyUsed(t: global.Tree, r: global.Tree): String => Boolean = {
    index = global.ask {() => GlobalIndex(r)}
    s =>
      global.ask { () =>
        !doesNameCollide(s, t.symbol).isEmpty
      }
  }

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

    val alreadyUsed = nameAlreadyUsed(valA, tree)

    assertFalse(alreadyUsed("c"))
    assertFalse(alreadyUsed("Whatever"))
    assertFalse("x is not visible from a", alreadyUsed("x"))
    assertFalse(alreadyUsed("A"))
    assertFalse(alreadyUsed("method2"))

    assertTrue (alreadyUsed("b"))
    assertTrue (alreadyUsed("a"))
    assertTrue (alreadyUsed("method1"))
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

    val method1InA = tree.find("method1").get

    val alreadyUsed = nameAlreadyUsed(method1InA, tree)

    assertFalse(alreadyUsed("blabla"))
    assertFalse(alreadyUsed("method4"))

    assertTrue(alreadyUsed("aString"))
    assertTrue(alreadyUsed("method3"))
    assertTrue(alreadyUsed("method1"))
    assertTrue(alreadyUsed("method2"))
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

    val alreadyUsed = nameAlreadyUsed(valA, tree)

    assertFalse(alreadyUsed("Ok1"))
    assertFalse(alreadyUsed("Ok2"))
    assertFalse(alreadyUsed("B"))

    assertTrue(alreadyUsed("C1"))
    assertTrue(alreadyUsed("C2"))
    assertTrue(alreadyUsed("C3"))
    assertTrue(alreadyUsed("C4"))
  }
}

