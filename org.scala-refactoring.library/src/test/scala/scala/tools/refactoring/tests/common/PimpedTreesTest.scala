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
    
    assertFalse(originalRightSibling(c).isDefined)
    assertTrue(originalLeftSibling(c).isDefined)
  }
  
  @Test
  def templateNoSiblings() {
    
    val c = tree.find(_.isInstanceOf[Template]).get
    
    assertTrue(originalLeftSibling(c).isDefined)
    assertFalse(originalRightSibling(c).isDefined)
  }
    
  @Test
  def parentChain() {
    
    val i = tree.find(_.toString == "42").get
    
    val root = originalParentOf(i) flatMap (originalParentOf(_) flatMap (originalParentOf(_) flatMap originalParentOf)) get
    
    assertTrue(root.isInstanceOf[PackageDef])
  }
  
  @Test
  def rootHasNoParent() {
    assertEquals(None, originalParentOf(tree))
  }
  
  @Test
  def testSiblings() {
    
    val v = tree.find(_.isInstanceOf[ValDef]).get
    val actual = originalParentOf(v).get.toString.replaceAll("\r\n", "\n")
    
    assertTrue(actual.contains("<empty> {\n  private[this] val test: Int = 42;\n  private[this] val test2: Int = 42\n}"))
    
    assertEquals(None, originalLeftSibling(v))
    assertEquals("private[this] val test2: Int = 42", originalRightSibling(v).get.toString)
  }
}

