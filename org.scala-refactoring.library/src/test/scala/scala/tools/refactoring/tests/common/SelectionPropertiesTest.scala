package scala.tools.refactoring.tests.common

import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.common.ReplaceableSelections
import org.junit.Assert._

class SelectionPropertiesTest extends TestHelper with ReplaceableSelections {
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
  def outboundLocalDeps = {
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
  def noOutboundLocalDeps = {
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
  def outboundDepsInParameterLists = {
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
  def outboundDepsInTemplateScope = {
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
}