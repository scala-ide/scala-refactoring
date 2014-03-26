package scala.tools.refactoring.tests.common

import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.tests.util.TestHelper

import org.junit.Assert._

class SelectionPropertiesTest extends TestHelper with Selections {
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
  def representsValue() = {
    val sel = """
      object O{
        def fn = {
          /*(*/val i = 100
          i * 2/*)*/
    	}
      }
      """.selection
    assertTrue(sel.representsValue)
  }

  @Test
  def doesNotRepresentValue() = {
    val sel = """
      object O{
        def fn = {
          /*(*/val i = 100
          val b = i * 2/*)*/
    	}
      }
      """.selection
    assertFalse(sel.representsValue)
  }

  @Test
  def nonValuePatternsDoNotRepresentValues() = {
    val selWildcard = """object O { 1 match { case /*(*/_/*)*/ => () } }""".selection
    assertFalse(selWildcard.representsValue)

    val selCtorPattern = """object O { Some(1) match { case /*(*/Some(i)/*)*/ => () } }""".selection
    assertFalse(selCtorPattern.representsValue)

    val selBinding =  """object O { 1 match { case /*(*/i: Int/*)*/ => i } }""".selection
    assertFalse(selBinding.representsValue)

    val selPatAndGuad = """object O { 1 match { case /*(*/i if i > 10/*)*/ => i } }""".selection
    assertFalse(selPatAndGuad.representsValue)
  }

  @Test
  def valuePatternsDoRepresentValues() = {
    val selCtorPattern = """object O { Some(1) match { case /*(*/Some(1)/*)*/ => () } }""".selection
    assertTrue(selCtorPattern.representsValue)
  }

  @Test
  def argumentLists() = {
    val sel = """
      object O{
        def fn = {
          List(/*(*/1, 2/*)*/, 3)
    	}
      }
      """.selection
    assertFalse(sel.representsValue)
    assertFalse(sel.representsValueDefinitions)
    assertTrue(sel.representsArgument)
  }

  @Test
  def parameter() = {
    val sel = """
      object O{
        def fn(/*(*/a: Int/*)*/) = {
          a
    	}
      }
      """.selection
    assertFalse(sel.representsValue)
    assertTrue(sel.representsValueDefinitions)
    assertTrue(sel.representsParameter)
  }

  @Test
  def multipleParameters() = {
    val sel = """
      object O{
        def fn(/*(*/a: Int, b: Int/*)*/) = {
          a * b
    	}
      }
      """.selection
    assertFalse(sel.representsValue)
    assertTrue(sel.representsValueDefinitions)
    assertTrue(sel.representsParameter)
  }

  @Test
  def triggersSideEffects() = {
    val sel = """
      object O{
        var a = 1
        /*(*/def fn = {
    	  a += 1
          a
        }/*)*/
      }
      """.selection
    assertTrue(sel.mayHaveSideEffects)
  }
}