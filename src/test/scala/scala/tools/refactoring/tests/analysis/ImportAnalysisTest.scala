package scala.tools.refactoring.tests.analysis

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.analysis.ImportAnalysis
import org.junit.Assert._

class ImportAnalysisTest extends TestHelper with ImportAnalysis {
  import global._

  @Test
  def importTrees() = global.ask { () =>
    val tree = treeFrom("""
    import scala.collection.immutable.{LinearSeq, _}

    object O{
      import scala.math._
      val a = 1
    }

    object P{
      import collection.mutable._
      import O.a
    }
    """)

    val it = buildImportTree(tree)
    assertEquals("{scala.collection.immutable.List{scala.`package`._{scala.Predef._{scala.collection.immutable.LinearSeq{scala.collection.immutable._{scala.math._{}, scala.collection.mutable._{O.a{}}}}}}}}", it.toString())
  }

  @Test
  def isImported() = global.ask { () =>
    val tree = treeFrom("""
    import scala.math.E

    object O{
      def fn = {
        import scala.math.Pi
        99 * Pi * E
      }
    }
    """)

    val it = buildImportTree(tree)
    val piRef = findSymTree(tree, "value Pi")
    val eRef = findSymTree(tree, "value E")
    val fnDef = findSymTree(tree, "method fn")

    assertTrue(it.isImportedAt(piRef.symbol, piRef.pos))
    assertTrue(it.isImportedAt(eRef.symbol, eRef.pos))

    assertFalse(it.isImportedAt(piRef.symbol, fnDef.pos))
    assertTrue(it.isImportedAt(eRef.symbol, fnDef.pos))
  }

  @Test
  def isImportedWithWildcard() = global.ask { () =>
    val tree = treeFrom("""
    object O{
      def fn = {
        import scala.math._
        99 * Pi * E
      }
    }
    """)

    val it = buildImportTree(tree)
    val piRef = findSymTree(tree, "value Pi")
    val eRef = findSymTree(tree, "value E")
    val fnDef = findSymTree(tree, "method fn")

    assertTrue(it.isImportedAt(piRef.symbol, piRef.pos))
    assertTrue(it.isImportedAt(eRef.symbol, eRef.pos))

    assertFalse(it.isImportedAt(piRef.symbol, fnDef.pos))
  }

  @Test
  def predefsAreAlwaysImported() = global.ask { () =>
    val tree = treeFrom("""
    object O{
      println(123)
      List(1, 2, 3)
    }
    """)

    val it = buildImportTree(tree)
    val printlnRef = findSymTree(tree, "method println")
    val listRef = findSymTree(tree, "object List")

    val oDef = findSymTree(tree, "object O")

    assertTrue(it.isImportedAt(printlnRef.symbol, oDef.pos))
    assertTrue(it.isImportedAt(listRef.symbol, oDef.pos))
  }

  @Test
  @Ignore
  def importsOfValueMembers() = global.ask { () =>
    val tree = treeFrom("""
    package pkg
    object O{
      val a = new{ val b = 1 }

      import a._

      def fn = b
    }
    """)

    val it = buildImportTree(tree)
    val bRef = findSymTree(findSymTree(tree, "method fn"), "value b")

    val oDef = findSymTree(tree, "object O")
    val aDef = findSymTree(tree, "value a")

    assertFalse(it.isImportedAt(bRef.symbol, oDef.pos))
    assertFalse(it.isImportedAt(bRef.symbol, aDef.pos))

    assertTrue(it.isImportedAt(bRef.symbol, bRef.pos))
  }

  def findSymTree(t: Tree,s: String) =
    t.collect{
      case t: SymTree if t.symbol.toString == s => t
    }.head
}
