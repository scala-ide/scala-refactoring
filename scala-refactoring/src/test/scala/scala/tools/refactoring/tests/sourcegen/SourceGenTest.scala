/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests.sourcegen

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.sourcegen._
import scala.tools.nsc.ast.Trees

// FIXME operations tests
@Test
class SourceGenTest extends TestHelper with SourceGen with LayoutHelper with Formatting with AstTransformations {
  
  implicit def stringToPrettyPrint(original: String) = new {
    def cleanTree(s: String) = (removeAuxiliaryTrees andThen emptyAllPositions)(treeFrom(s)).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generate(cleanTree(original)).get)
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

