package scala.tools.refactoring.tests.sourcegen

import scala.tools.refactoring.sourcegen.CommentsUtils
import org.junit.Assert.assertEquals
import org.junit.Test

class CommentsUtilsTest {
  @Test
  def testStripCommentWithExampleFromTicket1002166() {
    val source = """
      object Class {
        ("//")
        ("//")
      }
    """

   assertEquals(source, CommentsUtils.stripComment(source))
  }

  @Test
  def testStripCommentWithMinimalExampleFromTicket1002166() {
    val source = """("\\")"""
    assertEquals(source, CommentsUtils.stripComment(source))
  }

  @Test
  def testSplitCommentWithSimpleExamples() {
    testSplitComment("", "", "")
    testSplitComment("//", "  ",  "//")
    testSplitComment("""/**/""", "    ", "/**/")
    testSplitComment("a = 3//test", "a = 3      ", "     //test")
    testSplitComment("/**/a/*b*/c/*d*/e//f", "    a     c     e   ", "/**/ /*b*/ /*d*/ //f")
  }

  @Test
  def testSplitCommentWithMultilineExamples() {
    val tripleQuote = "\"\"\""
    
    /*
     * Attention:
     *  These test cases contain a lot of relevant white space chars; make sure that they are not stripped
     *  away by safe actions.
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
             
         }
         """,
         """
                  
           //
          
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
              
                            
                            
                 
                 
               
            def x = 43
                
                
          }
          """,
          """
                   
            /*
             * /***********/
             * /***********/
             * //
             * //
             */
                      
            ////
            ////
           
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
              
              
               
          """,
          s"""
                     
              
              
               
               
            //
            /*
             */
          """
      ) :: Nil


      testCases.foreach((testSplitComment _).tupled)
  }

  private def testSplitComment(in: String, woComments: String, commentsOnly: String): Unit = {
    val (resWoComments, resCommentsOnly) = CommentsUtils.splitComment(in)
    assertEquals(s"woComments: $in", woComments, resWoComments)
    assertEquals(s"commentsOnly: $in", commentsOnly, resCommentsOnly)
  }
}
