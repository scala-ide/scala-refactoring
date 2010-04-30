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
class PimpedTreesTest extends TestHelper with PimpedTrees with AstTransformations {
  
  def treeForFile(file: AbstractFile) = {
    global.unitOfFile.get(file) map (_.body) flatMap removeAuxiliaryTrees
  }
  
  import global._
  
  val tree = treeFrom("""
    package treetest

    class Test {
      val test = 42
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
    
    assertFalse(c.originalLeftSibling.isDefined)
    assertFalse(c.originalRightSibling.isDefined)
  }
    
  @Test
  def parentChain() {
    
    val i = tree.find(_.toString == "42").get
    
    val root = i.originalParent flatMap (_.originalParent flatMap (_.originalParent flatMap (_.originalParent))) get
    
    assertEquals(tree, root)
  }
  
  @Test
  def rootHasNoParent() {
    assertEquals(None, tree.originalParent)
  }
  
  
  @Test
  def testSiblings() {
    
    val v = tree.find(_.isInstanceOf[ValDef]).get
    
    assertEquals(
        """java.lang.Object with ScalaObject {
  def this(): treetest.Test = {
    Test.super.this();
    ()
  };
  private[this] val test: Int = 42;
  <stable> <accessor> def test: Int = Test.this.test
}""", v.originalParent.get.toString)
    
    assertEquals(
        """def this(): treetest.Test = {
  Test.super.this();
  ()
}""", v.originalLeftSibling.get.toString)

    assertEquals("<stable> <accessor> def test: Int = Test.this.test", v.originalRightSibling.get.toString)
  }

}

