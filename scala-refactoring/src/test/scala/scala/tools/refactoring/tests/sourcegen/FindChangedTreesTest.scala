/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests.sourcegen

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.common._
import scala.tools.refactoring.sourcegen._
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.io.AbstractFile

@Test
class FindChangedTreesTest extends TestHelper with PimpedTrees with CustomTrees with AstTransformations with ConsoleTracing {
  
  def treeForFile(file: AbstractFile) = {
    global.unitOfFile.get(file) map (_.body) flatMap removeAuxiliaryTrees
  }
  
  import global._
  import Transformations._
  
  val reverseBody = Transformations.transform[Tree, Tree] {
    case t: Template => t.copy(body = t.body.reverse) setPos t.pos
  }
  
  val doubleAllDefNames = Transformations.transform[Tree, Tree] {
    case t: DefDef => t.copy(name = t.name.toString + t.name.toString) setPos t.pos
  }
  
  val tree = treeFrom("""
    package treetest

    class Test {
      def test = 42
      val test2 = 42
    }
    """)
  
  @Test
  def findchange() {
    
    def hasTreeInternallyChanged(t: Tree): Boolean = findOriginalTree(t) map (t → _) getOrElse(return true) match {
      case (t: NameTree, o: NameTree) => 
        t.nameString != o.nameString
      case _ => 
        false
    }
    
    def hasChangedChildren(t: Tree): Boolean = findOriginalTree(t) map children match {
      case None => Predef.error("should never happen")
      case Some(origChld) =>
        val chld = children(t)
        if(chld.size != origChld.size) return false
        (chld zip origChld) exists {
          case (c, oc) => 
            !c.sameTree(oc)
        }
    }
    
    def searchChildrenForChanges(t: Tree): List[Tree] = {
      children(t) flatMap findAllChangedTrees flatMap (_._2) 
    }
    
    def findAllChangedTrees(t: Tree): List[(Tree, List[Tree])] = {
      
      if (hasTreeInternallyChanged(t)) {
        trace("Tree %s has changed internally.", t.getClass.getSimpleName)
        List(t → (t :: searchChildrenForChanges(t)))
      } else if (hasChangedChildren(t)) {
        trace("Tree %s has changed children.", t.getClass.getSimpleName)
        List(t → (t :: searchChildrenForChanges(t)))
      } else {
        trace("Tree %s has no changes, searching in children.", t.getClass.getSimpleName)
        children(t) flatMap findAllChangedTrees
      }
    }
    
    val t = removeAuxiliaryTrees &> ↓(⊆(reverseBody)) apply tree get
    
    val x = findAllChangedTrees(t)
    
    println(x)
  }
}






