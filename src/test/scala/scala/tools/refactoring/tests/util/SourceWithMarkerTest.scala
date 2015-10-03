package scala.tools.refactoring.tests.util
import org.junit.Test
import org.junit.Assert._
import scala.tools.refactoring.util.SourceWithMarker
import scala.language.implicitConversions
import scala.util.control.NonFatal

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
    val moveToStartOfMultilineComment = moveToBracketClose ~ ']' ~ (comment) ~ spaces
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

  val mvntsToTestAtEndOfString = {
    val mvnts =
      characterLiteral ::
      plainid ::
      op ::
      opChar ::
      stringLiteral ::
      comment ::
      comment.atLeastOnce ::
      bracket ::
      bracketsWithContents ::
      charToMovement('c') ::
      stringToMovement("s") ::
      symbolLiteral ::
      any ::
      none ::
      any.butNot('c') ::
      space ::
      Nil

    mvnts.zipWithIndex.map { case (m, i) => (m, s"mvnts[$i]") }
  }

  @Test
  def testWithEmptySource() {
    mvntsToTestAtEndOfString.foreach { case (mvnt, indexStr) =>
      try {
        assertTrue(indexStr, mvnt(SourceWithMarker()).isEmpty)
      } catch {
        case e: IllegalArgumentException =>
          throw new RuntimeException(s"Error at $indexStr", e)
      }
    }
  }

  @Test
  def testAtEndOfSource() {
    val src = SourceWithMarker("a")
    val prefixMvnt = stringToMovement("a")

    mvntsToTestAtEndOfString.foreach { case (mvnt, indexStr) =>
      assertTrue(indexStr, src.moveMarker(prefixMvnt ~ mvnt.zeroOrMore).isDepleted)
      assertTrue(indexStr, src.moveMarker((mvnt ~ prefixMvnt.zeroOrMore) | prefixMvnt.zeroOrMore).isDepleted)
    }
  }

  @Test
  def testCharConst() {
    val src1 = SourceWithMarker("(a='b',b=''',c='\'',e='s)")
    val baseMvnt1 = '(' ~ "a=" ~ characterLiteral ~ ",b=" ~ characterLiteral ~ ",c=" ~ characterLiteral ~ ",e="

    assertEquals("'", src1.moveMarker(baseMvnt1).current.toString)
    assertEquals("(", src1.moveMarker(baseMvnt1 ~ characterLiteral).current.toString)
    assertTrue(src1.moveMarker(baseMvnt1 ~ "'s)").isDepleted)

    val src2 = SourceWithMarker("""'\222'x''';'\b'°'\\'''\"'::""")
    val baseMvnt2 = (characterLiteral ~ any).zeroOrMore

    assertEquals(":", src2.moveMarker(baseMvnt2).current.toString)
    assertEquals("'", src2.moveMarker(baseMvnt2 ~ (any ~ characterLiteral ~ any).backward).current.toString)
  }

  @Test
  def testNtimes() {
    val src = SourceWithMarker("aaaa")
    assertTrue(src.moveMarker('a'.nTimes(4)).isDepleted)
    assertFalse(src.moveMarker('a'.nTimes(3)).isDepleted)
    assertFalse(src.moveMarker('a'.nTimes(5)).isDepleted)
  }

  @Test
  def testOpChar() {
    val src = SourceWithMarker("+-/*:><!~^\u03f6.")
    assertEquals("~", src.moveMarker(opChar.zeroOrMore ~ (opChar.nTimes(2) ~ '.').backward).current.toString)
  }

  @Test
  def testUntilWithSimpleExamples() {
    val src = SourceWithMarker("0123456789")
    assertEquals("5", src.moveMarker(until("5")).current.toString)
    assertEquals("5", src.moveMarker(until("5", skipping = digit)).current.toString)
    assertEquals("0", src.moveMarker(until("5", skipping = digit.zeroOrMore)).current.toString)
  }

  @Test
  def testStringLiteral() {
    val trippleQuote = "\"\"\""

    val src1 = SourceWithMarker(s"""$trippleQuote a $trippleQuote""")
    assertTrue(src1.moveMarker(stringLiteral).isDepleted)

    val src2 = SourceWithMarker(raw"""
      $trippleQuote
      asdfasdfadsf
      ""
      asdfasdf
      $trippleQuote
      "asdf\""
      "\n"
      "\t\"\"";
    """)

    val mvnt2 = (spaces ~ stringLiteral ~ spaces).atLeastOnce
    assertEquals(";", src2.moveMarker(mvnt2).current.toString)
  }

  @Test
  def testUntilWithTypicalExamples() {
    def untilVal(name: String) = until(name ~ spaces ~ "=", skipping = characterLiteral | symbolLiteral | stringLiteral | comment)

    val src1 = SourceWithMarker("(j = i, i = j)")
    val src2 = SourceWithMarker("""(j = "i = 2;", i = "v = 2"/*k=2*/,k=l)""")

    val trippleQuote = "\"\"\""
    val src3 = SourceWithMarker(s"""(a = $trippleQuote
      b = 0
      c = 0
      e = 0
      $trippleQuote, b = 'd_=, c = 'd', d= 3)
    )""")

    assertEquals("i", src1.moveMarker(untilVal("j") ~ any.nTimes(4)).current.toString)
    assertEquals("j", src1.moveMarker(untilVal("i") ~ any.nTimes(4)).current.toString)

    assertEquals("v", src2.moveMarker(untilVal("i") ~ any.nTimes(5)).current.toString)
    assertEquals("l", src2.moveMarker(untilVal("k") ~ any.nTimes(2)).current.toString)

    assertEquals("3", src3.moveMarker(untilVal("d") ~ any.nTimes(3)).current.toString)
  }

  @Test
  def testSeqOpsAtEndOfSource() {
    val src = SourceWithMarker("aaa")

    val shouldDeplete =
      ('a' ~ 'a' ~ 'a') ::
      ('a'.atLeastOnce) ::
      ('a'.zeroOrMore) ::
      (any ~ any ~ any) ::
      (('a' ~ 'a' ~ 'a' ~ 'a').zeroOrMore ~ 'a'.atLeastOnce) ::
      (('a' ~ 'a' ~ 'a') ~ 'a'.zeroOrMore) ::
      (('a' ~ 'a' ~ 'a' ~ 'a') | 'a'.nTimes(3)) ::
      Nil

    val shouldNotDeplete =
      ('a' ~ 'a' ~ 'a' ~ 'a') ::
      (('a' ~ 'a' ~ 'a') ~ 'a'.atLeastOnce) ::
      Nil

   shouldDeplete.foreach { mvnt =>
      assertTrue(src.moveMarker(mvnt).isDepleted)
    }

   shouldNotDeplete.foreach { mvnt =>
      assertFalse(src.moveMarker(mvnt).isDepleted)
    }
  }

  private implicit class SourceWithMarkerOps(underlying: SourceWithMarker) {
    def withMarkerOnLastChar = underlying.copy(marker = underlying.source.length - 1)
  }
}
