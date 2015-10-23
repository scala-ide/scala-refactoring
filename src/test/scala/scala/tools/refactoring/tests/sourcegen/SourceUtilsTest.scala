package scala.tools.refactoring.tests.sourcegen

import scala.tools.refactoring.sourcegen.SourceUtils
import org.junit.Assert.assertEquals
import org.junit.Test

class SourceUtilsTest {
  @Test
  def testStripCommentWithExampleFromTicket1002166(): Unit = {
    val source = """
      object Class {
        ("//")
        ("//")
      }
    """

   assertEquals(source, SourceUtils.stripComment(source))
  }

  @Test
  def testStripCommentWithMinimalExampleFromTicket1002166(): Unit = {
    val source = """("\\")"""
    assertEquals(source, SourceUtils.stripComment(source))
  }

  @Test
  def testSplitCommentWithSimpleExamples(): Unit = {
    testSplitComment("", "", "")
    testSplitComment("//", "  ",  "//")
    testSplitComment("""/**/""", "    ", "/**/")
    testSplitComment("a = 3//test", "a = 3      ", "     //test")
    testSplitComment("/**/a/*b*/c/*d*/e//f", "    a     c     e   ", "/**/ /*b*/ /*d*/ //f")
  }

  @Test
  def testSplitCommentWithMultilineExamples(): Unit = {
    val tripleQuote = "\"\"\""

    /*
     * Attention:
     *  These tests contain a lot of relevant trailing white space chars; they are marked with ¶
     *  so that they are not stripped away by safe actions.
     */
    val testCases =
      (
         """
         class X {
           //
         }
         """,
         """
         class X {
             ¶
         }
         """,
         """
                  ¶
           //
          ¶
         """
      ) ::
      (
          """
          class X {
            /*
             * /***********/
             * /***********/
             * //
             * //
             */
            def x = 43
            ////
            ////
          }
          """,
          """
          class X {
              ¶
                            ¶
                            ¶
                 ¶
                 ¶
               ¶
            def x = 43
                ¶
                ¶
          }
          """,
          """
                   ¶
            /*
             * /***********/
             * /***********/
             * //
             * //
             */
                      ¶
            ////
            ////
           ¶
          """
      ) ::
      (
          s"""
          val x = $tripleQuote
            //
            /*
             */
            $tripleQuote
            //
            /*
             */
          """,
          s"""
          val x = $tripleQuote
            //
            /*
             */
            $tripleQuote
              ¶
              ¶
               ¶
          """,
          s"""
                     ¶
              ¶
              ¶
               ¶
               ¶
            //
            /*
             */
          """
      ) :: Nil


      testCases.foreach((testSplitComment _).tupled)
  }

  @Test
  def testCountRelevantBracketsWithSimpleExamples() = {
    testCountRelevantBrackets("", 0, 0)
    testCountRelevantBrackets("val `(` = 42", 0, 0)
    testCountRelevantBrackets("""val x = ")"""", 0, 0)
    testCountRelevantBrackets("""val x = "\")"""", 0, 0)
    testCountRelevantBrackets("def x = 3", 0, 0)
    testCountRelevantBrackets("def f(x: Int) = x", 1, 1)
    testCountRelevantBrackets("def f(x: Int) = x //(", 1, 1)
    testCountRelevantBrackets("def f(x: Int) = /*)*/ x //(", 1, 1)
    testCountRelevantBrackets("""val z = "\"()()()\"()()()\"" """, 0, 0)
    testCountRelevantBrackets("'('", 0, 0)
    testCountRelevantBrackets("""(''', '\'', '(', ')')""", 1, 1)
  }

  @Test
  def testCountRelevantBracketsWithMultilineExamples() = {
    val tripleQuote = "\"\"\""

    val testCases =
      (
          """
          /*
           * ()
           */
          """, 0, 0
       ) ::
       (
           raw"""
           //))))))))))))))))))))))))))
           val x = "))))))))))))))))))))))"
           val y = $tripleQuote
             )))))))))))))))))))))))))))))))))))))))
             $tripleQuote
           /* /**/ /* -- */
            *)))))))))))))))))))))))))))))))))
            */
           val z = "\")))))))))))))))))))))))))))))))"

           val c = '('

           val `))))))` = '\)'

           ())
           """, 1, 2
       ) :: Nil

    testCases.foreach((testCountRelevantBrackets _).tupled)
  }

  private def testCountRelevantBrackets(in: String, expectedOpen: Int, expectedClose: Int): Unit = {
    val res = SourceUtils.countRelevantBrackets(in)
    assertEquals((expectedOpen, expectedClose), res)
  }

  private def testSplitComment(in: String, woComments: String, commentsOnly: String): Unit = {
    val (resWoComments, resCommentsOnly) = SourceUtils.splitComment(removeTrailingSpaceMarkers(in))
    assertEquals(s"woComments: $in", removeTrailingSpaceMarkers(woComments), resWoComments)
    assertEquals(s"commentsOnly: $in", removeTrailingSpaceMarkers(commentsOnly), resCommentsOnly)
  }

  private def removeTrailingSpaceMarkers(str: String) = str.replace("¶", "")
}
