package scala.tools.refactoring.tests.analysis

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.analysis.ImportAnalysis
import org.junit.Assert._

class ImportAnalysisTest extends TestHelper with ImportAnalysis{
  import global._

  @Test
  def importTrees = {
    val s = toSelection("""
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
    
    val it = buildImportTree(s.root)
    assertEquals("{scala.collection.immutable.List{scala.`package`._{scala.Predef._{scala.collection.immutable.LinearSeq{scala.collection.immutable._{scala.math._{}, scala.collection.mutable._{O.a{}}}}}}}}", it.toString())
  }

  @Test
  def isImported = {
    val s = toSelection("""
    import scala.math.E
        
    object O{
      def fn = {
        import scala.math.Pi
        99 * Pi * E
      }
    }
    """)
    
    val it = buildImportTree(s.root)
    val piRef = findSymTree(s.root, "value Pi")
    val eRef = findSymTree(s.root, "value E")
    val fnDef = findSymTree(s.root, "method fn")
    
    assertTrue(it.isImportedAt(piRef.symbol, piRef.pos))
    assertTrue(it.isImportedAt(eRef.symbol, eRef.pos))
    
    assertFalse(it.isImportedAt(piRef.symbol, fnDef.pos))
    assertTrue(it.isImportedAt(eRef.symbol, fnDef.pos))
  }

  @Test
  def isImportedWithWildcard = {
    val s = toSelection("""
    object O{
      def fn = {
        import scala.math._
        99 * Pi * E
      }
    }
    """)
    
    val it = buildImportTree(s.root)
    val piRef = findSymTree(s.root, "value Pi")
    val eRef = findSymTree(s.root, "value E")
    val fnDef = findSymTree(s.root, "method fn")
    
    assertTrue(it.isImportedAt(piRef.symbol, piRef.pos))
    assertTrue(it.isImportedAt(eRef.symbol, eRef.pos))
    
    assertFalse(it.isImportedAt(piRef.symbol, fnDef.pos))
  }

  @Test
  def predefsAreAlwaysImported = {
    val s = toSelection("""
    object O{
      println(123)
      List(1, 2, 3)
    }
    """)
    
    val it = buildImportTree(s.root)
    val printlnRef = findSymTree(s.root, "method println")
    val listRef = findSymTree(s.root, "object List")
    
    val oDef = findSymTree(s.root, "object O")
    
    assertTrue(it.isImportedAt(printlnRef.symbol, oDef.pos))
    assertTrue(it.isImportedAt(listRef.symbol, oDef.pos))
  }

  @Test
  @Ignore
  def importsOfValueMembers = {
    val s = toSelection("""
    package pkg
    object O{
      val a = new{ val b = 1 }

      import a._
      
      def fn = b
    }
    """)
    
    val it = buildImportTree(s.root)
    val bRef = findSymTree(findSymTree(s.root, "method fn"), "value b")
    
    val oDef = findSymTree(s.root, "object O")
    val aDef = findSymTree(s.root, "value a")
    
    assertFalse(it.isImportedAt(bRef.symbol, oDef.pos))
    assertFalse(it.isImportedAt(bRef.symbol, aDef.pos))

    assertTrue(it.isImportedAt(bRef.symbol, bRef.pos))
  }
  
  def findSymTree(t: Tree,s: String) =
    t.collect{
      case t: SymTree if t.symbol.toString == s => t
    }.head
}