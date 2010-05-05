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
import scala.tools.refactoring.common._
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.io.AbstractFile

@Test
class AstTransformationsTest extends TestHelper with AstTransformations with PimpedTrees {
  
  import global._
  
  def treeForFile(file: AbstractFile) = {
    global.unitOfFile.get(file) map (_.body) flatMap removeAuxiliaryTrees
  }
  
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
    
    val newTree = (removeAuxiliaryTrees &> emptyAllPositions)(tree).get
    
    assertFalse(newTree.exists(_.pos != NoPosition))
  }
  
  @Test 
  def associativity() = {
    
    import Transformations.{succeed, fail}

    assertTrue ((  succeed |>  succeed  &> fail[String]  )("").isDefined)
    assertTrue ((  succeed |> (succeed  &> fail[String]) )("").isDefined)
    assertFalse(( (succeed |>  succeed) &> fail[String]  )("").isDefined)
  } 
  
  @Test 
  def allChildrenAreCalled() = {
    
    var out = List[String]()
    
    val t = Transformations.transform [Tree, Tree] {
      case t => 
        out ::= t.getClass.getSimpleName
        t
    }
        
    val tree = treeFrom("""
    package ab
    trait A
    trait B
    """)
    
    Transformations.all(t) apply tree
    
    assertEquals("Ident, ClassDef, ClassDef", out.reverse mkString ", ")
  }  
  
  @Test 
  def allAbortsEarly() = {
    
    var out = List[String]()
    
    val onlyIdent = Transformations.transform [Tree, Tree] {
      case t: Ident => 
        out ::= t.getClass.getSimpleName
        t
    }
        
    val tree = treeFrom("""
    package ab
    trait A
    trait B
    """)
    
    assertTrue(Transformations.all(onlyIdent) apply tree isEmpty)
    assertEquals("Ident", out.reverse mkString ", ")
  }  
  
  @Test 
  def topDownIsDepthFirst() = {
    
    var out = List[String]()
    
    val t = Transformations.transform [Tree, Tree] {
      case t => 
        out ::= t.getClass.getSimpleName
        t
    }
        
    val tree = treeFrom("""
    package ab
    trait A
    trait B
    """)
    
    Transformations.topdown(t) apply tree
    assertEquals("PackageDef, Ident, ClassDef, Template, Select, Ident, ClassDef, Template, Select, Ident", out.reverse mkString ", ")
  }  
  
  
  @Test 
  def bottomUp() = {
    
    var out = List[String]()
    
    val t = Transformations.transform [Tree, Tree] {
      case t => 
        out ::= t.getClass.getSimpleName
        t
    }
        
    val tree = treeFrom("""
    package ab
    trait A
    trait B
    """)
    
    Transformations.bottomup(t) apply tree
    assertEquals("Ident, Ident, Select, Template, ClassDef, Ident, Select, Template, ClassDef, PackageDef", out.reverse mkString ", ")
  }  
  
  
}

