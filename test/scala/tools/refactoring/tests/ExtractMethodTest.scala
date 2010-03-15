package scala.tools.refactoring.tests

import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.SilentTracing
import scala.tools.refactoring.ExtractMethod
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test

class ExtractMethodTest extends TestHelper with TestRefactoring {
    
  implicit def stringToRefactoring(src: String) = new TestRefactoringImpl(src) {
    val refactoring = new ExtractMethod(global) with /*Silent*/Tracing
    def extractMethod(name: String, e: String) = doIt(e, new refactoring.RefactoringParameters {
      val methodName = name
    })
  }

  //@Test
  def extractBlock = """
    class A {
      def extractFrom: Int = {
/*(*/   val a = {
          val b = 1
          b * 5
        }   /*)*/
        a * a
      }
    }
    """ extractMethod("prntln",
    """
    class A {
      def extractFrom: Int = {
        val a = prntln
        a * a
      }
      def prntln(): Int = {
/*(*/   val a = {
          val b = 1
          b * 5
        }   /*)*/
        a
      }
    }
    """)
    
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
  def ignoreOtherClass = """
    class A {
      def extractFrom {
/*(*/   println("hello")/*)*/
        ()
      }
    }

    class A2(s: String)
    class B(s: String) extends A2(s)
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

    class A2(s: String)
    class B(s: String) extends A2(s)
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
    
  @Test
  def extractIfElseTry = """
    class A {
      def extractFrom(): Boolean = {
        if(true == true) /*(*/  true  /*)*/
        else {
          try {
            println("hello world")
            if(true) true
            else false
          } catch {
            case _ => false
          }
        }
      }
    }
    """ extractMethod("test",
    """
    class A {
      def extractFrom(): Boolean = {
        if(true == true) /*(*/  test
        else {
          try {
            println("hello world")
            if(true) true
            else false
          } catch {
            case _ => false
          }
        }
      }
      def test(): Boolean = {
        true  /*)*/
      }
    }
    """)    

  @Test
  def extractCheckForFalse = """
trait Check {
  def whatIsIt(check: Boolean) {
    if (/*(*/check == false/*)*/ /*hi*/)
      println("It's false")
    else
      println("It's true")
  }
}
    """ extractMethod("isFalse",
    """
trait Check {
  def whatIsIt(check: Boolean) {
    if (isFalse(check))
      println("It's false")
    else
      println("It's true")
  }
  def isFalse(check: Boolean): Boolean = {
  /*(*/check == false/*)*/ /*hi*/
  }
}
    """)    
    
  @Test
  def extractWithMethod = """
    class A {
      def extractFrom(): Boolean = {
        val invert: Boolean => Boolean = ! _
        val a = false
/*(*/   val b = invert(a)    /*)*/
        b
      }
    }
    """ extractMethod("certainlyTrue",
    """
    class A {
      def extractFrom(): Boolean = {
        val invert: Boolean => Boolean = ! _
        val a = false
        val b = certainlyTrue(invert, a)
        b
      }
      def certainlyTrue(invert: (Boolean) => Boolean, a: Boolean): Boolean = {
/*(*/   val b = invert(a)    /*)*/
        b
      }
    }
    """)    

  @Test
  def extractAllAtOnce = """
object C {
  def calculate {
    val sumList: Seq[Int] => Int = _ reduceLeft (_+_)
    val prodList: Seq[Int] => Int = _ reduceLeft (_*_)
    val values = 1 to 10 toList
/*(*/    val     sum = sumList(values)   // the sum
    val product = prodList(values) /*)*/ // the product

    println("The sum from 1 to 10 is "+ sum +"; the product is "+ product)
  }
}
  """ extractMethod("magic",
  """
object C {
  def calculate {
    val sumList: Seq[Int] => Int = _ reduceLeft (_+_)
    val prodList: Seq[Int] => Int = _ reduceLeft (_*_)
    val values = 1 to 10 toList
    val (sum, product) = magic(sumList, prodList, values)

    println("The sum from 1 to 10 is "+ sum +"; the product is "+ product)
  }
  def magic(sumList: (Seq[Int]) => Int, prodList: (Seq[Int]) => Int, values: List[Int]): (Int, Int) = {
/*(*/    val     sum = sumList(values)   // the sum
    val product = prodList(values) /*)*/ // the product
    (sum, product)
  }
}
  """)
  
  @Test
  def extractLarger = """
object C {
  def whatIsIt(check: Boolean) {
    if (/*(*/check == false/*)*/ /*hi*/)
      println("It's false")
    else
      println("It's true")
  }

  def unrelated1 {
    println("unrelated1")
  }

  def unrelated2 {
    println("unrelated2")
  }

  def unrelated3 {
    println("unrelated3")
  }
}

object c2 {
  def blabla {
    println("blabla")
  }
}
  """ extractMethod("isFalse",
  """
object C {
  def whatIsIt(check: Boolean) {
    if (isFalse(check))
      println("It's false")
    else
      println("It's true")
  }
  def isFalse(check: Boolean): Boolean = {
  /*(*/check == false/*)*/ /*hi*/
  }

  def unrelated1 {
    println("unrelated1")
  }

  def unrelated2 {
    println("unrelated2")
  }

  def unrelated3 {
    println("unrelated3")
  }
}

object c2 {
  def blabla {
    println("blabla")
  }
}
  """)
}
