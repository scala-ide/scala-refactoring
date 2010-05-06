/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests.sourcegen

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert
import org.junit.Assert._
import scala.tools.refactoring.sourcegen._
import scala.tools.refactoring.common._
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.io.AbstractFile

@Test
class SourceGenTest extends TestHelper with SourceGen with LayoutHelper with Formatting with AstTransformations with ConsoleTracing {
  
  import global._
  import Transformations._
  
  def treeForFile(file: AbstractFile) = {
    unitOfFile get file map (_.body) flatMap removeAuxiliaryTrees
  }
  
  implicit def stringToPrettyPrint(original: String) = new {
    def cleanTree(s: String) = (removeAuxiliaryTrees &> emptyAllPositions)(treeFrom(s)).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generate(cleanTree(original)).get)
  }
  
  implicit def treeToPrettyPrint(original: Tree) = new {
    def cleanTree(t: Tree) = (removeAuxiliaryTrees &> emptyAllPositions)(t).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generate(cleanTree(original)).get)
  }
  
  val reverseBody = Transformations.transform[Tree, Tree] {
    case t: Template => t.copy(body = t.body.reverse) setPos t.pos
  }
  
  val doubleAllDefNames = Transformations.transform[Tree, Tree] {
    case t: DefDef => t.copy(name = t.name.toString + t.name.toString) setPos t.pos
  }
  
  @Test
  def testObjectTemplates() = {
    
  }
  
  @Test
  def testSelfTypes() = {

    val tree = treeFrom("""
    trait ATrait {
      self =>
    }
    trait BTrait {
      self: ATrait =>
    }
    trait CTrait {
      self: BTrait with ATrait =>
    }
    """)
    
    assertEquals("""
    trait ATrait {
      self =>
    }
    trait BTrait {
      self: ATrait =>
    }
    trait CTrait {
      self: BTrait with ATrait =>
    } 
    """, generate(removeAuxiliaryTrees apply tree get).get)
  }
  
  @Test
  def testClassTemplates() = {
    
    val tree = treeFrom("""
    trait ATrait
    class ASuperClass(x: Int, d: String)
    class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
      self_type_annotation =>
      def someMethod() {
      }
    }
    """)
    
    assertEquals("""
    trait ATrait
    class ASuperClass(x: Int, d: String)
    class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
      self_type_annotation =>
      def someMethod() {
      }
    }
    """, generate(removeAuxiliaryTrees apply tree get).get)
  }
  
  
  @Test
  def testImports() = {
    val tree = treeFrom("""
    import java.lang.{String => S}
    import java.lang.Object
    import java.lang.{String => S, Object => _, _}
    import scala.collection.mutable._
    """)

    assertEquals("""
    import java.lang.{String => S}
    import java.lang.Object
    import java.lang.{String => S, Object => _, _}
    import scala.collection.mutable._
    """, generate(removeAuxiliaryTrees apply tree get).get)
    
    tree prettyPrintsTo """package <empty>
import java.lang.{String => S}
import java.lang.Object
import java.lang.{String => S, Object => _, _}
import scala.collection.mutable._"""
  }
  
  @Test
  def testMethods() = {
    
    val tree = treeFrom("""
      trait ATest
      {
        def abcd[T](a: String, b: Int): Int
        def a() = 5
        def b = 42
        def ab(i: Int)(j: Int) = (i ,     j)
        def timesTwo(i: Int) = {
          i.*(5)
          i * 2
        }
        def square {
          def nested(i: Int) = {
            i * i
          }
          nested(5)
        }
      }
    """)

    assertEquals("""
      trait ATest
      {
        def squaresquare {
          def nestednested(i: Int) = {
            i * i
          }
          nested(5)
        }
        def timesTwotimesTwo(i: Int) = {
          i.*(5)
          i * 2
        }
        def abab(i: Int)(j: Int) = (i ,     j)
        def bb = 42
        def aa() = 5
        def abcdabcd[T](a: String, b: Int): Int
      }
    """, generate(removeAuxiliaryTrees &> ↓(⊆(doubleAllDefNames)) &> ↓(⊆(reverseBody)) apply tree get).get) 
  }
  
  @Test
  def testVals() = {
    val tree = treeFrom("""
    /*a*/package /*b*/xyz/*c*/ {
      // now a class
      trait A
      {
        val a: Int = 5 //a
        val b = "huhu" //b
      }
    }
    """)

    assertEquals("""
    /*a*/package /*b*/xyz/*c*/ {
      // now a class
      trait A
      {
        val b = "huhu" //b
        val a: Int = 5 //a
      }
    }
    """, generate(removeAuxiliaryTrees &> ↓(⊆(reverseBody)) apply tree get).get)
  }
  
  @Test
  def testMethodSignatures() = """
    package xy
    
    class A {
      def a(): Int
      def b(): Int = 5
      def c() = 5
      def d() = {
        val a = 5
        a
      }
      def e(i: Int) = i
      def f(i: Int)(j: Int) = i+j
      def g(i: Int, j: Int) = i+j
      def h(i: Int, j: Int): (Int, Int) = (i, j)
      def id[A](a: A) = a
    }
    """ prettyPrintsTo """package xy
class A {
def a: Int
def b: Int = 5
def c = 5
def d = {
val a = 5
a
}
def e(i: Int) = i
def f(i: Int)(j: Int) = i.+(j)
def g(i: Int, j: Int) = i.+(j)
def h(i: Int, j: Int): (Int, Int) = (i, j)
def id[A](a: A) = a
}
"""
}

