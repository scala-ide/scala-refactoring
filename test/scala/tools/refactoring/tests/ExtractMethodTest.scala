package scala.tools.refactoring.tests

import scala.tools.refactoring.ExtractMethod
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class ExtractMethodTest extends TestHelper {
  
  class StringExtractMethod(source: String) {
    def extractMethod(name: String, expected: String) = {
      val result = new ExtractMethod(global, compile(source), source.indexOf("/*(*/"), source.indexOf("/*)*/")) perform name
      assertEquals(expected, result)
    }
  }
  
  implicit def stringToStringExtractMethod(source: String) = new StringExtractMethod(source)

  @Test
  def simpleExtract = """
    class A {
      def extractFrom {
/*(*/   println("hello")/*)*/
        ()
      }
    }
    """ extractMethod("prntln",
    """
    class A {
      def extractFrom {
        prntln
        ()
      }
      def prntln(): Unit = {
/*(*/   println("hello")/*)*/
      }
    }
    """)

  @Test
  def simpleExtractOneParameter = """
    class A {
      def extractFrom {
        val a = 1
/*(*/   println(a)  /*)*/
        ()
      }
    }
    """ extractMethod("prntln",
    """
    class A {
      def extractFrom {
        val a = 1
        prntln(a)
        ()
      }
      def prntln(a: Int): Unit = {
/*(*/   println(a)  /*)*/
      }
    }
    """)

  @Test
  def simpleExtractSeveralParameters = """
    class A {
      def extractFrom(d: Int) {
        val a = 1
        val b = 1
        val c = 1
/*(*/   println(a + b + c + d)  /*)*/
        ()
      }
    }
    """ extractMethod("prntln",
    """
    class A {
      def extractFrom(d: Int) {
        val a = 1
        val b = 1
        val c = 1
        prntln(d, a, b, c)
        ()
      }
      def prntln(d: Int, a: Int, b: Int, c: Int): Unit = {
/*(*/   println(a + b + c + d)  /*)*/
      }
    }
    """)
    
  @Test
  def simpleExtractReturn = """
    class A {
      def extractFrom() {
/*(*/   val a = 1  /*)*/
        a
      }
    }
    """ extractMethod("prntln",
    """
    class A {
      def extractFrom() {
        val a = prntln
        a
      }
      def prntln(): Int = {
/*(*/   val a = 1  /*)*/
        a
      }
    }
    """)  
    
  @Test
  def simpleExtractMultipleReturns = """
    class A {
      def extractFrom() {
/*(*/   val a = 1
        val b = 1  /*)*/
        a + b
      }
    }
    """ extractMethod("prntln",
    """
    class A {
      def extractFrom() {
        val (a, b) = prntln
        a + b
      }
      def prntln(): (Int, Int) = {
/*(*/   val a = 1
        val b = 1  /*)*/
        (a, b)
      }
    }
    """)
    
  @Test
  def simpleExtractParametersAndReturns = """
    class A {
      def extractFrom() {
        val a = 1
        val b = 1
        val c = 1
/*(*/   val d = a + c
        val e = d + a  /*)*/
        a+b+c+d+e
      }
    }
    """ extractMethod("prntln",
    """
    class A {
      def extractFrom() {
        val a = 1
        val b = 1
        val c = 1
        val (d, e) = prntln(a, c)
        a+b+c+d+e
      }
      def prntln(a: Int, c: Int): (Int, Int) = {
/*(*/   val d = a + c
        val e = d + a  /*)*/
        (d, e)
      }
    }
    """)
    
  @Test
  def extractBlockExpression = """
    class A {
      def extractFrom(): Int = {
        val a = 1
/*(*/   a + 1    /*)*/
      }
    }
    """ extractMethod("inc",
    """
    class A {
      def extractFrom(): Int = {
        val a = 1
        inc(a)
      }
      def inc(a: Int): Int = {
/*(*/   a + 1    /*)*/
      }
    }
    """)
    
  @Test
  def replaceWholeMethod = """
    class A {
      def extractFrom(): Int = {
/*(*/   val a = 1
        a + 1    /*)*/
      }
    }
    """ extractMethod("inc",
    """
    class A {
      def extractFrom(): Int = {
        inc
      }
      def inc(): Int = {
/*(*/   val a = 1
        a + 1    /*)*/
      }
    }
    """)
    
  @Test
  def extractIfCond = """
    class A {
      def extractFrom(): Boolean = {
        if/*aa*/( /*(*/ true == true /*)*/ )
          true
        else
          false 
      }
    }
    """ extractMethod("test",
    """
    class A {
      def extractFrom(): Boolean = {
        if/*aa*/(test)
          true
        else
          false 
      }
      def test(): Boolean = {
      /*(*/ true == true /*)*/ 
      }
    }
    """)
        
  @Test
  def extractIfThen = """
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
 /*(*/    true /*)*/
        else
          false 
      }
    }
    """ extractMethod("test",
    """
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
          test
        else
          false 
      }
      def test(): Boolean = {
        /*(*/    true /*)*/
      }
    }
    """)  
    
  @Test
  def extractIfElse = """
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
          true
        else {
 /*(*/    false /*)*/
        }
      }
    }
    """ extractMethod("test",
    """
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
          true
        else {
          test
        }
      }
      def test(): Boolean = {
        /*(*/    false /*)*/
      }
    }
    """)
    
  @Test
  def extractIfSingleLineElse = """
    class A {
      def extractFrom(): Boolean = {
        if(true == true) true else /*(*/ false /*)*/
      }
    }
    """ extractMethod("test",
    """
    class A {
      def extractFrom(): Boolean = {
        if(true == true) true else /*(*/ test
      }
      def test(): Boolean = {
        false /*)*/
      }
    }
    """)    
}
