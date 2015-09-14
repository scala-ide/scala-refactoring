package scala.tools.refactoring.tests.util
import org.junit.Test
import org.junit.Assert._
import scala.tools.refactoring.util.SourceWithMarker
import scala.language.implicitConversions

class SourceWithMarkerTest {
  import SourceWithMarker.Movements._

  @Test
  def testApplyCharMovemnts() {
    val src = SourceWithMarker("implicit")
    assertEquals('m', src.moveMarker('i').current)
    assertEquals('l', src.moveMarker('u' | ('i' ~ 'm' ~ 'p')).current)
    assertEquals('i', src.moveMarker('i' ~ 'm'.backward).current)
  }


  @Test
  def testApplyCharAndStringMovements() {
    val src = SourceWithMarker("abstract")
    assertEquals('a', src.moveMarker("").current)
    assertEquals('a', src.moveMarker(('a' ~ "bs") ~ ('b' ~ "str").backward).current)
    assertTrue(src.moveMarker("abstrac" ~ "abstract".backward).isDepleted)
  }

  @Test
  def testApplyBasicMovements() {
    val src = SourceWithMarker("protected abstract override val x = 123")
    assertEquals('=', src.moveMarker((("protected" | "abstract" | "override" | "val" | "x" | "=") ~ spaces).zeroOrMore ~ "12" ~ (spaces ~ "123").backward).current)
  }

  @Test
  def testCommentsAndSpaces() {
    val src1 = SourceWithMarker("""//--
      val x = 3
    """)

    val src2 = SourceWithMarker("/**/val x = 3")

    val src3 = SourceWithMarker("""
      //--
      //--/**/------------->>
      //--

      /*
       * /**/
       */
      val test = 3
    """)

    val src4 = SourceWithMarker("x//", 2)

    val srcStr5 = "/**/ //**/"
    val src5 = SourceWithMarker(srcStr5, srcStr5.size - 1)

    val res = src5.moveMarker(commentsAndSpaces.backward)

    assertEquals("x", src4.moveMarker(commentsAndSpaces.backward).current.toString)
    assertEquals("v", src1.moveMarker(commentsAndSpaces).current.toString)
    assertEquals("v", src2.moveMarker(commentsAndSpaces).current.toString)
    assertEquals("v", src3.moveMarker(commentsAndSpaces).current.toString)
    assertTrue(src5.moveMarker(commentsAndSpaces.backward).isDepleted)
  }

  @Test
  def testSpaces() {
    val src1 = SourceWithMarker(" s")
    val src2 = SourceWithMarker("\n\ts")
    val src3 = SourceWithMarker("""
      s    s                s
    """)

    assertEquals("s", src1.moveMarker(spaces).current.toString)
    assertEquals("s", src2.moveMarker(spaces).current.toString)
    assertEquals("s", src3.moveMarker(spaces ~ 's' ~ spaces ~ 's' ~ spaces).current.toString)
    assertTrue(src1.moveMarker(spaces ~ (spaces ~ "s").backward).isDepleted)
  }

  @Test
  def testApplyWithMoreComplexExample() {
    val src = SourceWithMarker("""protected //--
      [test /**/]//--
      /*
       /*
        /**/
        */
      */ override val test = 99""")

    val moveToBracketOpen = "protected" ~ commentsAndSpaces
    val moveToBracketClose = moveToBracketOpen ~ bracketsWithContents ~ '/'.backward
    val moveToStartOfMultilineComment = moveToBracketClose ~ ']' ~ (consumeComment) ~ spaces
    val moveToEndOfMultilineComment = moveToStartOfMultilineComment ~ comments ~ (spaces ~ 'o').backward
    val moveToVal = moveToBracketClose ~ ']' ~ commentsAndSpaces ~ "override" ~ commentsAndSpaces

    assertEquals("[", src.moveMarker(moveToBracketOpen).current.toString)
    assertEquals("]", src.moveMarker(moveToBracketClose).current.toString)
    assertEquals("/", src.moveMarker(moveToStartOfMultilineComment).current.toString)
    assertEquals("/", src.moveMarker(moveToEndOfMultilineComment).current.toString)
    assertEquals("v", src.moveMarker(moveToVal).current.toString)

    assertTrue(src.moveMarker("protected" ~ ("protected" ~ commentsAndSpaces).backward).isDepleted)
    assertTrue(src.moveMarker(moveToVal ~ (moveToVal ~ "v").backward).isDepleted)
  }

  @Test
  def testWithRealisticExamples() {
    val srcStr = """
      package bug
      class Bug {
        private/*--*/ //**//**//**//**//**/
        // -/**/-
        // -/**/-
        [/**/ bug /**/] val /*(*/z/*)*/ = 99
      }
      """

    val src = SourceWithMarker(srcStr, srcStr.lastIndexOf("]"))
    val mvmt = (("private" | "protected") ~ commentsAndSpaces ~ bracketsWithContents).backward

    assertEquals("p", src.moveMarker(mvmt ~ commentsAndSpaces).current.toString)
  }

  @Test
  def testWithScopedAccessModifiers() {
    val src = SourceWithMarker("private[test]").withMarkerOnLastChar
    assertTrue(src.moveMarker((("private" | "protected") ~ commentsAndSpaces ~ bracketsWithContents).backward).isDepleted)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testCtorWithTooLargeMarker() {
    SourceWithMarker(Array(), 1)
  }

  @Test
  def testWithEmptySource() {
    assertTrue(SourceWithMarker().isDepleted)
  }

  private implicit def stringToCharArray(str: String): Array[Char] = str.toCharArray
  private implicit class SourceWithMarkerOps(underlying: SourceWithMarker) {
    def withMarkerOnLastChar = underlying.copy(marker = underlying.source.length - 1)
  }
}
