package scala.tools.refactoring.tests.common

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Assert._
import scala.tools.refactoring.common.Selections

class SelectionDependenciesTest extends TestHelper with Selections {
  import global._

  @Test
  def inboundDeps() = {
    val sel = toSelection("""
      object O{
        val i = 1
        def fn(p: Int) = {
          /*(*/val a = p * i
          println(a)/*)*/
          a
        }
      }
      """)
    assertSymbols("value p, value i", sel.inboundLocalDeps)
  }

  @Test
  def inboundDepsDoesNotIncludeCalledMethods() = {
    val sel = toSelection("""
      object O{
        val i = 1
        /*(*/println(i.toInt*i*i.toInt)
        println(i.*(2)*(3))/*)*/
      }
      """)
    assertSymbols("value i", sel.inboundLocalDeps)
  }

  @Test
  def inboundDepsIncludesMethodArguments() = {
    val sel = toSelection("""
      object O{
        def fn(i: Int) = /*(*/8.*(i)/*)*/
      }
      """)
    assertSymbols("value i", sel.inboundLocalDeps)
  }

  @Test
  def inboundDepsDoesNotIncludeMembersOfLiterals() = {
    val sel = toSelection("""
      object O{
        /*(*/
    	1 + 2
    	1.toInt.toInt.toInt
    	/*)*/
      }
      """)
    assertSymbols("", sel.inboundLocalDeps)
  }

  @Test
  def inboundImplicitDeps() = {
    val sel = toSelection("""
      object O{
        implicit def wrapInt(i: Int) = new {
          def extension = i * 2
        }

        val i = /*(*/2.extension/*)*/
      }
      """)
    assertSymbols("method extension, method wrapInt", sel.inboundLocalDeps)
  }

  @Test
  def inboundTypeDeps() = {
    val sel = toSelection("""
      class A(i: Int)

      object N{
        def apply() = 1
      }

      object O{
        val i = 1
        def fn = {
          /*(*/new A(N())/*)*/
        }
      }
      """)
    assertSymbols("constructor A, class A, method apply, object N", sel.inboundLocalDeps)
  }

  @Test
  def inboundTypeDepsByOwner() = {
    val sel = toSelection("""
      object N{
        def apply() = 1
      }

      object O{
        val i = 1
        def fn = {
          /*(*/N() * i/*)*/
        }
      }
      """)
    val symO = sel.expandTo[DefDef].get.enclosingTree.symbol.ownerChain.find(_.isType).get
    assertSymbols("value i", sel.inboundDepsOwnedBy(symO))
  }

  @Test
  def inboundDepsOfWildcardsInMatch() = {
    val sel = toSelection("""
      object O{
        /*(*/1 match{
    	  case _ => ()
        }/*)*/
      }
      """)
    assertSymbols("", sel.inboundLocalDeps)
  }

  @Test
  def inboundDepsOfImported() = {
    val sel = toSelection("""
      object O{
        import scala.math.Pi
    	import scala.collection.mutable

    	/*(*/(Pi, new mutable.LinkedList)/*)*/
      }
      """)
    assertSymbols("", sel.inboundLocalDeps)
  }

  @Test
  def outboundLocalDeps() = {
    val sel = toSelection("""
      object O{
        def fn(p: Int) = {
          /*(*/val (a, b) = (1, p)
          val c = a/*)*/
          (b, c)
        }
      }
      """)
    assertSymbols("value b, value c", sel.outboundLocalDeps)
  }

  @Test
  def noOutboundLocalDeps() = {
    val sel = toSelection("""
      object O{
        def fn(c: Int) = {
    	  {
            /*(*/val (a, b) = (1, p)
            val c = a/*)*/
          }
          c
        }
      }
      """)
    assertSymbols("", sel.outboundLocalDeps)
  }

  @Test
  def outboundDepsInParameterLists() = {
    val sel = toSelection("""
      object O{
        def fn(/*(*/c: Int, d: Int/*)*/) = {
          c
        }
      }
      """)
    assertSymbols("value c", sel.outboundLocalDeps)
  }

  @Test
  def outboundDepsInTemplateScope() = {
    val sel = toSelection("""
      object O{
        def fn = fm
        /*(*/def fm = 1
        def fo = 2/*)*/
        def fq = fo
      }
      """)
    assertSymbols("method fm, method fo", sel.outboundLocalDeps)
  }

  @Test
  def outboundImportedDeps() = {
    val sel = toSelection("""
      object O{
        def fn = {
          /*(*/import scala.math.Pi/*)*/
    	  Pi
        }
      }
      """)
    assertSymbols("", sel.outboundLocalDeps)
  }

  @Test
  def reassignedVariablesAsOutboundDeps() = {
    val sel = toSelection("""
      object O{
        def fn = {
    	  var a = 1
          /*(*/a += 1/*)*/
    	  a
        }
      }
      """)
    assertSymbols("variable a", sel.outboundLocalDeps)
  }

  def assertSymbols(expected: String, actualSymbols: List[Symbol]) =
    assertEquals(expected, global.ask { () =>
      actualSymbols.mkString(", ")
    })
}