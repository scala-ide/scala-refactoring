/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.common

import tests.util.TestHelper
import org.junit.Assert._
import common.PimpedTrees

class PimpedTreesTest extends TestHelper with PimpedTrees {

  import global._

  def tree = treeFrom("""
    package treetest

    class Test {
      val test = 42
      val test2 = 42
    }

    """)

  @Test
  def classHasNoRightSibling() = global.ask { () =>

    val c = tree.find(_.isInstanceOf[ClassDef]).get

    assertFalse(originalRightSibling(c).isDefined)
    assertTrue(originalLeftSibling(c).isDefined)
  }

  @Test
  def templateNoSiblings() = global.ask { () =>

    val c = tree.find(_.isInstanceOf[Template]).get

    assertTrue(originalLeftSibling(c).isDefined)
    assertFalse(originalRightSibling(c).isDefined)
  }

  @Test
  def parentChain() = global.ask { () =>

    val i = tree.find(_.toString == "42").get

    val root = originalParentOf(i) flatMap (originalParentOf(_) flatMap (originalParentOf(_) flatMap originalParentOf))

    assertTrue(root.get.isInstanceOf[PackageDef])
  }

  @Test
  def rootHasNoParent() = global.ask { () =>
    assertEquals(None, originalParentOf(tree))
  }

  @Test
  def testSiblings() = global.ask { () =>

    val v = tree.find(_.isInstanceOf[ValDef]).get
    val actual = originalParentOf(v).get.toString.replaceAll("\r\n", "\n")

    assertTrue(actual.contains("private[this] val test: Int = 42;"))

    assertEquals(None, originalLeftSibling(v))
    assertEquals("private[this] val test2: Int = 42", originalRightSibling(v).get.toString)
  }

  @Test
  def namePositionOfFieldAccessor = {
    val src = """
    object O{
      val field = 1
    }
    """
    val root = treeFrom(src)

    val accessor = root.collect{
      case t: ValOrDefDef if t.name.decode == "field" => t
    }.head

    assertEquals(src.indexOf("field"), accessor.namePosition().point)
  }
}

