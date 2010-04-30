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
  import treetransformations._
  
  def treeForFile(file: AbstractFile) = {
    global.unitOfFile.get(file) map (_.body) flatMap removeAuxiliaryTrees
  }
  
  implicit def stringToPrettyPrint(original: String) = new {
    def cleanTree(s: String) = (removeAuxiliaryTrees andThen emptyAllPositions)(treeFrom(s)).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generate(cleanTree(original)).get)
  }
  
  @Test
  def testx() = {
    
    val tree = treeFrom("""
      /*a*/package /*b*/xyz/*c*/
      // now a class
      class A
      {
        val a: Int = 5 //a
        val b = "huhu" //b
      }
    """)

    val emptyBody = treetransformations.transform[Tree, Tree] {
      case t: Template => t.copy(body = t.body.reverse) setPos t.pos
    }
    
    println("«"+ generate(removeAuxiliaryTrees/* andThen topdown(some(emptyBody))*/ apply tree get).get +"»")
    
    Assert.fail()
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

