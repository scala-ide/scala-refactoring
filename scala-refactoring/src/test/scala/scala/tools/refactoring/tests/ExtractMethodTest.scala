/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests

import scala.tools.refactoring.analysis.IndexImplementations
import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.{ConsoleTracing, SilentTracing}
import scala.tools.refactoring.implementations.ExtractMethod
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test

class ExtractMethodTest extends TestHelper with TestRefactoring {
  outer =>
    
  implicit def stringToRefactoring(src: String) = {
    val pro = new FileSet {
      add(src, src)
    }
    
    new TestRefactoringImpl(pro) {
      val refactoring = new ExtractMethod with SilentTracing with IndexImplementations {
        val global = outer.global
        val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
        val index = GlobalIndex(cuIndexes) 
      }
      def extractMethod(name: String, e: String) = doIt(e, new refactoring.RefactoringParameters {
        val methodName = name
      })
    }
  }

  @Test
  def extractBlock = """
    package extractBlock
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
    package extractBlock
    class A {
      def extractFrom: Int = {
        val a = prntln
        a * a
      }
      def prntln: Int = {
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
    package simpleExtract
    class A {
      def extractFrom {
/*(*/   println("hello")/*)*/
        ()
      }
    }
    """ extractMethod("myOwnPrint",
    """
    package simpleExtract
    class A {
      def extractFrom {
        myOwnPrint
        ()
      }
      def myOwnPrint: Unit = {
/*(*/   println("hello")/*)*/
      }
    }
    """)
    
  @Test
  def ignoreOtherClass = """
    package ignoreOtherClass
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
    package ignoreOtherClass
    class A {
      def extractFrom {
        prntln
        ()
      }
      def prntln: Unit = {
/*(*/   println("hello")/*)*/
      }
    }

    class A2(s: String)
    class B(s: String) extends A2(s)
    """)

  @Test
  def simpleExtractOneParameter = """
    package simpleExtractOneParameter
    class A {
      def extractFrom {
        val a = 1
/*(*/   println(a)  /*)*/
        ()
      }
    }
    """ extractMethod("prntln",
    """
    package simpleExtractOneParameter
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
    package simpleExtractSeveralParameters
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
    package simpleExtractSeveralParameters
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
    package simpleExtractReturn
    class A {
      def extractFrom() {
/*(*/   val a = 1  /*)*/
        a
      }
    }
    """ extractMethod("prntln",
    """
    package simpleExtractReturn
    class A {
      def extractFrom() {
        val a = prntln
        a
      }
      def prntln: Int = {
/*(*/   val a = 1  /*)*/
        a
      }
    }
    """)  
    
  @Test
  def simpleExtractMultipleReturns = """
    package simpleExtractMultipleReturns
    class A {
      def extractFrom() {
/*(*/   val a = 1
        val b = 1  /*)*/
        a + b
      }
    }
    """ extractMethod("prntln",
    """
    package simpleExtractMultipleReturns
    class A {
      def extractFrom() {
        val (a, b) = prntln
        a + b
      }
      def prntln: (Int, Int) = {
/*(*/   val a = 1
        val b = 1  /*)*/
        (a, b)
      }
    }
    """)
    
  @Test
  def simpleExtractParametersAndReturns = """
    package simpleExtractParametersAndReturns
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
    package simpleExtractParametersAndReturns
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
    package extractBlockExpression
    class A {
      def extractFrom(): Int = {
        val a = 1
/*(*/   a + 1    /*)*/
      }
    }
    """ extractMethod("inc",
    """
    package extractBlockExpression
    class A {
      def extractFrom(): Int = {
        val a = 1
        inc(a)    /*)*/
      }
      def inc(a: Int): Int = {
/*(*/   a + 1
      }
    }
    """)
    
  @Test
  def replaceWholeMethod = """
    package replaceWholeMethod
    class A {
      def extractFrom(): Int = {
/*(*/   val a = 1
        a + 1    /*)*/
      }
    }
    """ extractMethod("inc",
    """
    package replaceWholeMethod
    class A {
      def extractFrom(): Int = {
        inc    /*)*/
      }
      def inc: Int = {
/*(*/   val a = 1
        a + 1
      }
    }
    """)
    
  @Test
  def extractIfCond = """
    package extractIfCond
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
    package extractIfCond
    class A {
      def extractFrom(): Boolean = {
        if/*aa*/(test)
          true
        else
          false 
      }
      def test: Boolean = {
         /*(*/ true == true /*)*/ 
      }
    }
    """)
        
  @Test
  def extractIfThen = """
    package extractIfThen
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
    package extractIfThen
    class A {
      def extractFrom(): Boolean = {
        if(true == true)  test /*)*/
        else
          false 
      }
      def test: Boolean = {
 /*(*/    true
      }
    }
    """)  
    
  @Test
  def extractIfElse = """
    package extractIfElse
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
    package extractIfElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
          true
        else {
 /*(*/      test /*)*/
        }
      }
      def test: Boolean = {
        false
      }
    }
    """)  
    
  @Test
  def extractIfSingleLineElse = """
    package extractIfSingleLineElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true) true else /*(*/ false /*)*/
      }
    }
    """ extractMethod("test",
    """
    package extractIfSingleLineElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true) true else /*(*/   test /*)*/
      }
      def test: Boolean = {
        false
      }
    }
    """)    
    
  @Test
  def extractIfElseTry = """
    package extractIfElseTry
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
    package extractIfElseTry
    class A {
      def extractFrom(): Boolean = {
        if(true == true)  test  /*)*/
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
      def test: Boolean = {
         /*(*/  true
      }
    }
    """)    

  @Test
  def extractCheckForFalse = """
    package extractCheckForFalse
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
    package extractCheckForFalse
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
    package extractWithMethod
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
    package extractWithMethod
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
    package extractAllAtOnce
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
    package extractAllAtOnce
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
    package extractLarger
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
    package extractLarger
    object C {
      def whatIsIt(check: Boolean) {
        if (isFalse(check))
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
      def isFalse(check: Boolean): Boolean = {
        /*(*/check == false/*)*/ /*hi*/
      }
    }
    
    object c2 {
      def blabla {
        println("blabla")
      }
    }
  """)
}
