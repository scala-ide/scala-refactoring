package scala.tools.refactoring.tests.common

import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.tests.util.TestHelper

import org.junit.Assert._

class SelectionPropertiesTest extends TestHelper with Selections {
  import global._

  @Test
  def representsValue() = {
    val sel = toSelection("""
      object O{
        def fn = {
          /*(*/val i = 100
          i * 2/*)*/
    	}
      }
      """)
    assertTrue(sel.representsValue)
  }

  @Test
  def doesNotRepresentValue() = {
    val sel = toSelection("""
      object O{
        def fn = {
          /*(*/val i = 100
          val b = i * 2/*)*/
    	}
      }
      """)
    assertFalse(sel.representsValue)
  }

  @Test
  def nonValuePatternsDoNotRepresentValues() = {
    val selWildcard = toSelection("""object O { 1 match { case /*(*/_/*)*/ => () } }""")
    assertFalse(selWildcard.representsValue)

    val selCtorPattern = toSelection("""object O { Some(1) match { case /*(*/Some(i)/*)*/ => () } }""")
    assertFalse(selCtorPattern.representsValue)

    val selBinding = toSelection("""object O { 1 match { case /*(*/i: Int/*)*/ => i } }""")
    assertFalse(selBinding.representsValue)

    val selPatAndGuad = toSelection("""object O { 1 match { case /*(*/i if i > 10/*)*/ => i } }""")
    assertFalse(selPatAndGuad.representsValue)
  }

  @Test
  def valuePatternsDoRepresentValues() = {
    val selCtorPattern = toSelection("""object O { Some(1) match { case /*(*/Some(1)/*)*/ => () } }""")
    assertTrue(selCtorPattern.representsValue)
  }

  @Test
  def argumentLists() = {
    val sel = toSelection("""
      object O{
        def fn = {
          List(/*(*/1, 2/*)*/, 3)
    	}
      }
      """)
    assertFalse(sel.representsValue)
    assertFalse(sel.representsValueDefinitions)
    assertTrue(sel.representsArgument)
  }

  @Test
  def parameter() = {
    val sel = toSelection("""
      object O{
        def fn(/*(*/a: Int/*)*/) = {
          a
    	}
      }
      """)
    assertFalse(sel.representsValue)
    assertTrue(sel.representsValueDefinitions)
    assertTrue(sel.representsParameter)
  }

  @Test
  def multipleParameters() = {
    val sel = toSelection("""
      object O{
        def fn(/*(*/a: Int, b: Int/*)*/) = {
          a * b
    	}
      }
      """)
    assertFalse(sel.representsValue)
    assertTrue(sel.representsValueDefinitions)
    assertTrue(sel.representsParameter)
  }

  @Test
  def triggersSideEffects() = global.ask { () =>
    val sel = toSelection("""
      object O{
        var a = 1
        /*(*/def fn = {
    	  a += 1
          a
        }/*)*/
      }
      """)
    assertTrue(sel.mayHaveSideEffects)
  }
}