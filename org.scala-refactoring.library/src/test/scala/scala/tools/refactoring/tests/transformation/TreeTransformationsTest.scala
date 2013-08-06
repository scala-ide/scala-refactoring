/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.transformation

import tests.util.TestHelper
import org.junit.Assert._
import common.PimpedTrees
import language.{postfixOps, reflectiveCalls}
import scala.tools.nsc.util.FailedInterrupt
import scala.tools.refactoring.common.SilentTracing

class TreeTransformationsTest extends TestHelper with SilentTracing {

  import global._

  def assertAllRangesOrNoPosition(t: Tree) =  assertFalse(t.exists(t => !(t.pos.isRange || t.pos == global.NoPosition)))

  @Test
  def allEmpty() = {

    val tree = treeFrom("""
    package xy
    case class AllEmpty(i: Int, a: String)
    """)

    val newTree = cleanTree(tree)

    assertFalse(newTree.exists(_.pos != NoPosition))
  }

  @Test
  def associativity() = {

    assertTrue ((  succeed |>  succeed  &> fail[String]  )("").isDefined)
    assertTrue ((  succeed |> (succeed  &> fail[String]) )("").isDefined)
    assertFalse(( (succeed |>  succeed) &> fail[String]  )("").isDefined)
  }

  @Test
  def allChildrenAreCalled() = global.ask { () =>

    var out = List[String]()

    val t = transform {
      case t =>
        out ::= t.getClass.getSimpleName
        t
    }

    val tree = treeFrom("""
    package ab
    trait A
    trait B
    """)

    allChildren(t) apply tree

    assertEquals("Ident, ClassDef, ClassDef", out.reverse mkString ", ")
  }

  @Test
  def allAbortsEarly() = global.ask { () =>

    var out = List[String]()

    val onlyIdent = transform {
      case t: Ident =>
        out ::= t.getClass.getSimpleName
        t
    }

    val tree = treeFrom("""
    package ab
    trait A
    trait B
    """)

    assertTrue(allChildren(onlyIdent) apply tree isEmpty)
    assertEquals("Ident", out.reverse mkString ", ")
  }

  @Test
  def topDownIsDepthFirst() = global.ask { () =>

    var out = List[String]()

    val t = transform {
      case t =>
        out ::= t.getClass.getSimpleName
        t
    }

    val tree = treeFrom("""
    package ab
    trait A
    trait B
    """)

    topdown(t) apply tree
    assertEquals("PackageDef, Ident, ClassDef, Template, Select, Ident, ClassDef, Template, Select, Ident", out.reverse mkString ", ")
  }

  @Test
  def bottomUp() = global.ask { () =>

    var out = List[String]()

    val t = transform {
      case t =>
        out ::= t.getClass.getSimpleName
        t
    }

    val tree = treeFrom("""
    package ab
    trait A
    trait B
    """)

    bottomup(t) apply tree
    assertEquals("Ident, Ident, Select, Template, ClassDef, Ident, Select, Template, ClassDef, PackageDef", out.reverse mkString ", ")
  }

  @Test(expected = classOf[FailedInterrupt])
  def topdownCanDiverge(): Unit = global.ask { () =>

    var out = List[String]()

    val t = transform {
      case t @ Block(stats, expr) => t copy (stats = t :: stats)
    }

    val tree = treeFrom("""
    class Class {
      def block {
        println("this is a block")
        println("this is a block")
      }
    }
    """)

    topdown(matchingChildren(t)) apply tree
  }

  @Test
  def bottomUpDoesNotDiverge() = global.ask { () =>


    var out = List[String]()

    val t = transform {
      case t @ Block(stats, expr) => t copy (stats = t :: stats)
    }

    val tree = treeFrom("""
    class Class {
      def block {
        println("this is a block")
        println("this is a block")
      }
    }
    """)

    val res = bottomup(matchingChildren(t)) apply tree
    assertFalse(res.get.isEmpty)
  }

  @Test
  def testReplaceTrees() = {
    val ts = List(1, 2, 3, 4, 5)

    assertEquals(List(1, 6, 3, 4, 5),    ts.replaceSequence(2 :: Nil, 6 :: Nil))
    assertEquals(List(6, 2, 3, 4, 5),    ts.replaceSequence(1 :: Nil, 6 :: Nil))
    assertEquals(List(1, 2, 3, 4, 6, 7), ts.replaceSequence(5 :: Nil, 6 :: 7 :: Nil))
    assertEquals(List(1, 2, 3, 4, 5),    ts.replaceSequence(6 :: Nil, 1 :: Nil))

    assertEquals(List(2, 1, 1), List(1, 1, 1).replaceSequence(1 :: Nil, 2 :: Nil))
  }
}
