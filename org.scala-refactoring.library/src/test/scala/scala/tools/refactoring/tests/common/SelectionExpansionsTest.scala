package scala.tools.refactoring.tests.common

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Assert._
import scala.tools.refactoring.common.Selections

class SelectionExpansionsTest extends TestHelper with Selections {
  import global._

  @Test
  def expandSelectionToBlock =
    """
    class C{
      def fn = {
        val a = 1
        val b = {
          val c = 2
          val d = /*(*/3/*)*/
        }
      }
    }
    """.assertExpansion(_.expandTo[Block].get).toBecome {
      """
    class C{
      def fn = {
        val a = 1
        val b = /*(*/{
          val c = 2
          val d = 3
        }/*)*/
      }
    }
    """
    }

  @Test
  def expandSelectionInForEnumerator =
    """
    class C{
      for{
        i <- List(1, 2, /*(*/3/*)*/)
      } println(i)
    }
    """.assertExpansion(_.expandTo[Tree].get).toRemain

  @Test
  def expandSelectionInYield =
    """
    class C{
      for{
        i <- List(1, 2, 3)
      } yield i * /*(*/1/*)*/
    }
    """.assertExpansion(_.expandTo[Tree].get).toRemain

  @Test
  def expandWithLastTreePartiallySelected =
    """
    class C{
      val first = 1
      /*(*/val a = 1
      val b = {
        val c = 2/*)*/
        val d = 3
        c * d
      }
      val last = 1
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      val first = 1
      /*(*/val a = 1
      val b = {
        val c = 2
        val d = 3
        c * d
      }/*)*/
      val last = 1
    }
    """
    }

  @Test
  def expandWithFirstTreePartiallySelected =
    """
    class C{
      def fn = {
        val a = 1
        /*(*/a
      }
      val b = 2/*)*/
      val c = 3
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      /*(*/def fn = {
        val a = 1
        a
      }
      val b = 2/*)*/
      val c = 3
    }
    """
    }

  @Test
  def expandWithNothingSelected =
    """
    class C{
      val a = 1/*(*//*)*/
    }
    """.assertExpansion(_.expand).toRemain

  @Test
  def expandValidSelection =
    """
    class C{
      val a = /*(*/1/*)*/
    }
    """.assertExpansion(_.expand).toRemain

  @Test
  def expandValidSelectionOfBlock =
    """
    class C{
      val a = /*(*/{
    	val b = 1
        b
      }/*)*/
    }
    """.assertExpansion(_.expand).toRemain

  @Test
  def expandMethodCalls =
    """
    class C{
      val s = "abc"
      s./*(*/mkString(", ")/*)*/
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      val s = "abc"
      /*(*/s.mkString(", ")/*)*/
    }
    """
    }

  @Test
  def expandPartiallySelectedMethodDefinitions1 =
    """
    class C{
      def /*(*/fn(i: Int) = {
        i/*)*/
      }
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      /*(*/def fn(i: Int) = {
        i
      }/*)*/
    }
    """
    }

  @Test
  def expandPartiallySelectedMethodDefinitions2 =
    """
    class C{
      def /*(*/fn(i: Int) = {
        i
      }/*)*/
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      /*(*/def fn(i: Int) = {
        i
      }/*)*/
    }
    """
    }

  @Test
  def expandPartiallySelectedMethodDefinitions3 =
    """
    class C{
      def /*(*/fn = {
        123
      }/*)*/
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      /*(*/def fn = {
        123
      }/*)*/
    }
    """
    }

  @Test
  def expandPartiallySelectedIf =
    """
    class C{
      if(/*(*/true)
    	1/*)*/
      else
        2
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      /*(*/if(true)
    	1
      else
        2/*)*/
    }
    """
    }

  @Test
  def expandPartiallySelectedMatch =
    """
    class C{
      1 match{
    	case 1 => /*(*/"a"
    	case 2 => "b"
      }/*)*/
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      /*(*/1 match{
    	case 1 => "a"
    	case 2 => "b"
      }/*)*/
    }
    """
    }

  @Test
  def expandSelectedPatternWithGuard =
    """
    class C{
      1 match{
    	case /*(*/i if i > /*)*/0 => i
      }
    }
    """.assertExpansion(_.expand).toBecome {
      """
    class C{
      1 match{
    	/*(*/case i if i > 0 => i/*)*/
      }
    }
    """
    }

  @Test
  def expandAnonFunWithWildcardParam =
    """
    class C{
      List(1).map(/*(*/_ + 1/*)*/)
    }
    """.withSelection { orig =>
      val expanded = orig.expand
      val fn = orig.root.collect {
        case t: Function => t
      }.head

      assertEquals(fn, expanded.selectedTopLevelTrees.head)
      assertTrue(fn.pos.sameRange(expanded.pos))
      assertEquals(fn, expanded.enclosingTree)
    }

  implicit class StringToSel(src: String) {
    def withSelection[T](fn: Selection => T) = {
      val tree = treeFrom(src)
      val start = commentSelectionStart(src)
      val end = commentSelectionEnd(src)
      val selection = FileSelection(tree.pos.source.file, tree, start, end)

      fn(selection)
    }
    def assertExpansion(expansion: Selection => Selection) = {
      withSelection { sel =>
        val expandedSel = expansion(sel)
        new {
          def toBecome(expectedSrc: String) = {
            expectedSrc.withSelection { expectedSel =>
              val (expected, actual) = global.ask { () =>
                (expectedSel.selectedTopLevelTrees.toString(), expandedSel.selectedTopLevelTrees.toString())
              }
              assertEquals(expected, actual)
            }
          }

          def toRemain = toBecome(src)
        }
      }
    }
  }

  def modify(s: Selection, dStart: Int, dEnd: Int): Selection =
    new Selection {
      val root = s.root
      val file = s.file
      val pos = s.pos.withStart(s.pos.start + dStart).withEnd(s.pos.end + dEnd)
    }
}