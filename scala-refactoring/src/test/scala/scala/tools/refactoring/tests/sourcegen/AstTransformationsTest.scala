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

@Test
class AstTransformationsTest extends TestHelper with AstTransformations {
  
  import treetransformations._
  import global._
  
  def assertAllRangesOrNoPosition(t: Tree) =  assertFalse(t.exists(t => !(t.pos.isRange || t.pos == global.NoPosition)))
  
  @Test
  def removeAuxiliary() = {
    
    val tree = treeFrom("""
    package xyz
    class Test {      
      def a(): Int
      def b(): Int = 5
      def c() = 5
      def d() = {
        val a = 5
        a + 0
      }
    }
    """)
    
    assertAllRangesOrNoPosition(removeAuxiliaryTrees(tree).get)
  }
  
  @Test
  def removeAuxiliaryCaseClass() = {
    
    val tree = treeFrom("""
    package xy
    case class A(i: Int, a: String)
    """)
    
    assertAllRangesOrNoPosition(removeAuxiliaryTrees(tree).get)
  }
  
  
  @Test
  def allEmpty() = {
    
    val tree = treeFrom("""
    package xy
    case class A(i: Int, a: String)
    """)
    
    val newTree = (removeAuxiliaryTrees andThen emptyAllPositions)(tree).get
    
    assertFalse(newTree.exists(_.pos != NoPosition))
  }
}

