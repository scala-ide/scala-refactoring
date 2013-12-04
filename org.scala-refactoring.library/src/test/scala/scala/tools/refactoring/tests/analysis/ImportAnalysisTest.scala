package scala.tools.refactoring.tests.analysis

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.analysis.ImportAnalysis
import org.junit.Assert._

class ImportAnalysisTest extends TestHelper with ImportAnalysis{
  import global._

  @Test
  def buildImportTree = {
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
    
    val it = ImportTree.build(s.root)
    assertEquals("{scala.collection.immutable.LinearSeq{scala.collection.immutable._{scala.math._{}, scala.collection.mutable._{O.a{}}}}}", it.toString())
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
    
    val it = ImportTree.build(s.root)
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
    
    val it = ImportTree.build(s.root)
    val piRef = findSymTree(s.root, "value Pi")
    val eRef = findSymTree(s.root, "value E")
    val fnDef = findSymTree(s.root, "method fn")
    
    assertTrue(it.isImportedAt(piRef.symbol, piRef.pos))
    assertTrue(it.isImportedAt(eRef.symbol, eRef.pos))
    
    assertFalse(it.isImportedAt(piRef.symbol, fnDef.pos))
  }
  
  def findSymTree(t: Tree,s: String) =
    t.collect{
      case t: SymTree if t.symbol.toString == s => t
    }.head
}