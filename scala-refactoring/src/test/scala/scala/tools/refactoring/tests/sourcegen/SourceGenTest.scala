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
class SourceGenTest extends TestHelper with SourceGen with LayoutHelper with Formatting {
  
  @Test
  def test1() = {
    
    val tree = treeFrom("""
    package xy
    
    class A {
      def a(): Int
      def b(): Int = 5
      def c() = 5
      def d() = {
        val a = 5
        a + 0
      }
    }
    """)
    println(generate(tree).get)
    
  }
}

