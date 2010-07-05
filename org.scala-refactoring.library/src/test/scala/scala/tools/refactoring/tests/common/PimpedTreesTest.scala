/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.common

import tests.util.TestHelper
import org.junit.Assert._
import common.PimpedTrees

class PimpedTreesTest extends TestHelper with PimpedTrees {
  
  override def treeForFile(file: AbstractFile) = {
    global.unitOfFile.get(file) map (_.body) flatMap removeAuxiliaryTrees
  }
  
  import global._
  
  val tree = treeFrom("""
    package treetest

    class Test {
      val test = 42
      val test2 = 42
    }

    """)
  
  @Test
  def classHasNoRightSibling() {
    
    val c = tree.find(_.isInstanceOf[ClassDef]).get
    
    assertFalse(c.originalRightSibling.isDefined)
    assertTrue(c.originalLeftSibling.isDefined)
  }
  
  @Test
  def templateNoSiblings() {
    
    val c = tree.find(_.isInstanceOf[Template]).get
    
    assertTrue(c.originalLeftSibling.isDefined)
    assertFalse(c.originalRightSibling.isDefined)
  }
    
  @Test
  def parentChain() {
    
    val i = tree.find(_.toString == "42").get
    
    val root = i.originalParent flatMap (_.originalParent flatMap (_.originalParent flatMap (_.originalParent))) get
    
    assertTrue(root.isInstanceOf[PackageDef])
  }
  
  @Test
  def rootHasNoParent() {
    assertEquals(None, tree.originalParent)
  }
  
  
  @Test
  def testSiblings() {
    
    val v = tree.find(_.isInstanceOf[ValDef]).get
    
    assertEquals("""<empty> with <empty> {
  private[this] val test: Int = 42;
  private[this] val test2: Int = 42
}""", v.originalParent.get.toString)
    
    assertEquals(None, v.originalLeftSibling)
    assertEquals("private[this] val test2: Int = 42", v.originalRightSibling.get.toString)
  }

}

