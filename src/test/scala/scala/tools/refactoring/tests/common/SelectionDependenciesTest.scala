package scala.tools.refactoring.tests.common

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Assert._
import scala.tools.refactoring.common.Selections

class SelectionDependenciesTest extends TestHelper with Selections {
  import global._

  implicit class StringToSel(src: String) {
    val root = treeFrom(src)
    val selection = {
      val start = commentSelectionStart(src)
      val end = commentSelectionEnd(src)
      FileSelection(root.pos.source.file, root, start, end)
    }
  }

  @Test
  def inboundDeps() = global.ask { () =>
    val sel = """
      object O{
        val i = 1
        def fn(p: Int) = {
          /*(*/val a = p * i
          println(a)/*)*/
          a
        }
      }
      """.selection
    assertEquals("value p, value i", sel.inboundLocalDeps.mkString(", "))
  }

  @Test
  def inboundDepsDoesNotIncludeCalledMethods() = global.ask { () =>
    val sel = """
      object O{
        val i = 1
        /*(*/println(i.toInt*i*i.toInt)
        println(i.*(2)*(3))/*)*/
      }
      """.selection
    assertEquals("value i", sel.inboundLocalDeps.mkString(", "))
  }

  @Test
  def inboundDepsIncludesMethodArguments() = global.ask { () =>
    val sel = """
      object O{
        def fn(i: Int) = /*(*/8.*(i)/*)*/
      }
      """.selection
    assertEquals("value i", sel.inboundLocalDeps.mkString(", "))
  }

  @Test
  def inboundDepsDoesNotIncludeMembersOfLiterals() = global.ask { () =>
    val sel = """
      object O{
        /*(*/
        1 + 2
        1.toInt.toInt.toInt
        /*)*/
      }
      """.selection
    assertEquals("", sel.inboundLocalDeps.mkString(", "))
  }

  @Test
  def inboundImplicitDeps() = global.ask { () =>
    val sel = """
      object O{
        implicit def wrapInt(i: Int) = new {
          def extension = i * 2
        }

        val i = /*(*/2.extension/*)*/
      }
      """.selection
    assertEquals("method extension, method wrapInt", sel.inboundLocalDeps.mkString(", "))
  }

  @Test
  def inboundTypeDeps() = global.ask { () =>
    val sel = """
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
      """.selection
    assertEquals("constructor A, class A, method apply, object N", sel.inboundLocalDeps.mkString(", "))
  }

  @Test
  def inboundTypeDepsByOwner() = global.ask { () =>
    val sel = """
      object N{
        def apply() = 1
      }

      object O{
        val i = 1
        def fn = {
          /*(*/N() * i/*)*/
        }
      }
      """.selection
    val symO = sel.expandTo[DefDef].get.enclosingTree.symbol.ownerChain.find(_.isType).get
    assertEquals("value i", sel.inboundDepsOwnedBy(symO).mkString(", "))
  }

  @Test
  def inboundDepsOfWildcardsInMatch() = global.ask { () =>
    val sel = """
      object O{
        /*(*/1 match{
          case _ => ()
        }/*)*/
      }
      """.selection
    assertEquals("", sel.inboundLocalDeps.mkString(", "))
  }

  @Test
  def inboundDepsOfImported() = global.ask { () =>
    val sel = """
      object O{
        import scala.math.Pi
        import scala.collection.mutable

        /*(*/(Pi, new mutable.LinkedList)/*)*/
      }
      """.selection
    assertEquals("", sel.inboundLocalDeps.mkString(", "))
  }

  @Test
  def outboundLocalDeps() = global.ask { () =>
    val sel = """
      object O{
        def fn(p: Int) = {
          /*(*/val (a, b) = (1, p)
          val c = a/*)*/
          (b, c)
        }
      }
      """.selection
    assertEquals("value b, value c", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  def noOutboundLocalDeps() = global.ask { () =>
    val sel = """
      object O{
        def fn(c: Int) = {
          {
            /*(*/val (a, b) = (1, p)
            val c = a/*)*/
          }
          c
        }
      }
      """.selection
    assertEquals("", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  def outboundDepsInParameterLists() = global.ask { () =>
    val sel = """
      object O{
        def fn(/*(*/c: Int, d: Int/*)*/) = {
          c
        }
      }
      """.selection
    assertEquals("value c", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  def outboundDepsInTemplateScope() = global.ask { () =>
    val sel = """
      object O{
        def fn = fm
        /*(*/def fm = 1
        def fo = 2/*)*/
        def fq = fo
      }
      """.selection
    assertEquals("method fm, method fo", sel.outboundLocalDeps.mkString(", "))
  }

  @Test @Ignore("The implementation needs to be fixed in order to pass this test")
  def outboundDepsOfVars() = global.ask { () =>
    val sel = """
      object O{
        def fn = {
          var a = 1
          def inner = /*(*/a += 1/*)*/
          a
        }
      }
      """.selection
    assertEquals("variable a", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  def outboundImportedDeps() = global.ask { () =>
    val sel = """
      object O{
        def fn = {
          /*(*/import scala.math.Pi/*)*/
          Pi
        }
      }
      """.selection
    assertEquals("", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  def reassignedVariablesAsOutboundDeps() = global.ask { () =>
    val sel = """
      object O{
        def fn = {
          var a = 1
          /*(*/a += 1/*)*/
          a
        }
      }
      """.selection
    assertEquals("variable a", sel.outboundLocalDeps.mkString(", "))
  }
}
