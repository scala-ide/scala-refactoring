/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import tests.util.TestRefactoring
import implementations.ExtractMethod
import tests.util.TestHelper

import language.reflectiveCalls

class ExtractMethodTest extends TestHelper with TestRefactoring {

  def extract(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExtractMethod with SilentTracing with TestProjectIndex
    val changes = performRefactoring(name)
  }.changes

  @Test
  def extractBlock = new FileSet {
    """
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
    """ becomes
    """
    package extractBlock
    class A {
      def extractFrom: Int = {
        val a = prntln
        a * a
      }

      private def prntln: Int = {
/*(*/   val a = {
          val b = 1
          b * 5
        }   /*)*/
        a
      }
    }
    """
  } applyRefactoring extract("prntln")

  @Test
  def simpleExtract = new FileSet {
    """
    package simpleExtract
    class A {
      def extractFrom {
/*(*/   println("hello")/*)*/
        ()
      }
    }
    """ becomes
    """
    package simpleExtract
    class A {
      def extractFrom {
        myOwnPrint
        ()
      }

      private def myOwnPrint: Unit = {
/*(*/   println("hello")/*)*/
      }
    }
    """
  } applyRefactoring extract("myOwnPrint")

  @Test
  def ignoreOtherClass = new FileSet {
    """
    package ignoreOtherClass
    class A {
      def extractFrom {
/*(*/   println("hello")/*)*/
        ()
      }
    }

    class A2(s: String)
    class B(s: String) extends A2(s)
    """ becomes
    """
    package ignoreOtherClass
    class A {
      def extractFrom {
        prntln
        ()
      }

      private def prntln: Unit = {
/*(*/   println("hello")/*)*/
      }
    }

    class A2(s: String)
    class B(s: String) extends A2(s)
    """
  } applyRefactoring extract("prntln")

  @Test
  def simpleExtractOneParameter = new FileSet {
    """
    package simpleExtractOneParameter
    class A {
      def extractFrom {
        val a = 1
/*(*/   println(a)  /*)*/
        ()
      }
    }
    """ becomes
    """
    package simpleExtractOneParameter
    class A {
      def extractFrom {
        val a = 1
        prntln(a)
        ()
      }

      private def prntln(a: Int): Unit = {
/*(*/   println(a)  /*)*/
      }
    }
    """
  } applyRefactoring extract("prntln")

  @Test
  def extractRangeParameter = new FileSet {
      """
      object ExtractMethod3 {

        def main(args: Array[String]) {

          val sumList: Seq[Int] => Int = _ reduceLeft (_+_)

          val values = 1 to 10

          /*(*/val sum = sumList(values)/*)*/   // the sum

          println("The sum from 1 to 10 is "+ sum)
        }
      }
      """ becomes """
      object ExtractMethod3 {

        def main(args: Array[String]) {

          val sumList: Seq[Int] => Int = _ reduceLeft (_+_)

          val values = 1 to 10
          val sum = prntln(sumList, values)

          println("The sum from 1 to 10 is "+ sum)
        }

        private def prntln(sumList: Seq[Int] => Int, values: scala.collection.immutable.Range.Inclusive): Int = {

          /*(*/val sum = sumList(values)/*)*/   // the sum
          sum
        }
      }
      """
  } applyRefactoring extract("prntln")

  @Test
  def simpleExtractSeveralParameters = new FileSet {
    """
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
    """ becomes
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

      private def prntln(d: Int, a: Int, b: Int, c: Int): Unit = {
/*(*/   println(a + b + c + d)  /*)*/
      }
    }
    """
  } applyRefactoring extract("prntln")

  @Test
  def simpleExtractReturn = new FileSet {
    """
    package simpleExtractReturn
    class A {
      def extractFrom() {
/*(*/   val a = 1  /*)*/
        a
      }
    }
    """ becomes
    """
    package simpleExtractReturn
    class A {
      def extractFrom() {
        val a = prntln
        a
      }

      private def prntln: Int = {
/*(*/   val a = 1  /*)*/
        a
      }
    }
    """
  } applyRefactoring extract("prntln")

  @Test
  def simpleExtractMultipleReturns = new FileSet {
    """
    package simpleExtractMultipleReturns
    class A {
      def extractFrom() {
/*(*/   val a = 1
        val b = 1  /*)*/
        a + b
      }
    }
    """ becomes
    """
    package simpleExtractMultipleReturns
    class A {
      def extractFrom() {
        val (a, b) = prntln
        a + b
      }

      private def prntln: (Int, Int) = {
/*(*/   val a = 1
        val b = 1  /*)*/
        (a, b)
      }
    }
    """
  } applyRefactoring extract("prntln")

  @Test
  def simpleExtractParametersAndReturns = new FileSet {
    """
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
    """ becomes
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

      private def prntln(a: Int, c: Int): (Int, Int) = {
/*(*/   val d = a + c
        val e = d + a  /*)*/
        (d, e)
      }
    }
    """
  } applyRefactoring extract("prntln")

  @Test
  def extractBlockExpression = new FileSet {
      """
    package extractBlockExpression
    class A {
      def extractFrom(): Int = {
        val a = 1
/*(*/   a + 1    /*)*/
      }
    }
    """ becomes """
    package extractBlockExpression
    class A {
      def extractFrom(): Int = {
        val a = 1
        inc(a)
      }

      private def inc(a: Int): Int = {
/*(*/   a + 1    /*)*/
      }
    }
    """
  } applyRefactoring extract("inc")

  @Test
  def replaceWholeMethod = new FileSet {
    """
    package replaceWholeMethod
    class A {
      def extractFrom(): Int = {
/*(*/   val a = 1
        a + 1    /*)*/
      }
    }
    """ becomes """
    package replaceWholeMethod
    class A {
      def extractFrom(): Int = {
        inc
      }

      private def inc: Int = {
/*(*/   val a = 1
        a + 1    /*)*/
      }
    }
    """
  } applyRefactoring extract("inc")

  @Test
  def extractIfCond = new FileSet {
    """
    package extractIfCond
    class A {
      def extractFrom(): Boolean = {
        if/*aa*/( /*(*/ true == true /*)*/)
          true
        else
          false
      }
    }
    """ becomes
    """
    package extractIfCond
    class A {
      def extractFrom(): Boolean = {
        if/*aa*/(test)
          true
        else
          false
      }

      private def test: Boolean = {
         /*(*/ true == true /*)*/
      }
    }
    """
  } applyRefactoring extract("test")

  @Test
  def extractAnonFunction = new FileSet {
    """
object ExtractMethod3 {

  def main(args: Array[String]) {
    val start =  0
    /*(*/val end   = 10
    val sum = start to end reduceLeft ((x, y) => x + y)/*)*/
    println("The sum from %d to %d is %d".format(start, end, sum))
  }
}
    """ becomes
    """
object ExtractMethod3 {

  def main(args: Array[String]) {
    val start =  0
    val (end, sum) = test(start)
    println("The sum from %d to %d is %d".format(start, end, sum))
  }

  private def test(start: Int): (Int, Int) = {
    /*(*/val end   = 10
    val sum = start to end reduceLeft ((x, y) => x + y)/*)*/
    (end, sum)
  }
}
    """
  } applyRefactoring extract("test")

  @Test
  def extractAnonFunction2 = new FileSet {
    """
object ExtractMethod3 {

  def main(args: Array[String]) {
    val start =  0
    /*(*/val end   = 10
    val sum = start to end reduceLeft (_ + _)/*)*/
    println("The sum from %d to %d is %d".format(start, end, sum))
  }
}
    """ becomes
    """
object ExtractMethod3 {

  def main(args: Array[String]) {
    val start =  0
    val (end, sum) = test(start)
    println("The sum from %d to %d is %d".format(start, end, sum))
  }

  private def test(start: Int): (Int, Int) = {
    /*(*/val end   = 10
    val sum = start to end reduceLeft (_ + _)/*)*/
    (end, sum)
  }
}
    """
  } applyRefactoring extract("test")

  @Test
  def extractAnonFunction3 = new FileSet {
    """
object ExtractMethod3 {

  def add(x: Int, y: Int) = x + y

  def main(args: Array[String]) {
    /*(*/val sum = 0 to 10 reduceLeft add/*)*/
    println("The sum is %d".format(sum))
  }
}
    """ becomes
    """
object ExtractMethod3 {

  def add(x: Int, y: Int) = x + y

  def main(args: Array[String]) {
    val sum = test
    println("The sum is %d".format(sum))
  }

  private def test: Int = {
    /*(*/val sum = 0 to 10 reduceLeft add/*)*/
    sum
  }
}
    """
  } applyRefactoring extract("test")

  @Test
  def extractIfThen = new FileSet {
    """
    package extractIfThen
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
 /*(*/    true /*)*/
        else
          false
      }
    }
    """ becomes
    """
    package extractIfThen
    class A {
      def extractFrom(): Boolean = {
        if(true == true) test /*)*/
        else
          false
      }

      private def test: Boolean = {
 /*(*/    true
      }
    }
    """
  } applyRefactoring extract("test")

  @Test
  def extractIfElse = new FileSet {
    """
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
    """ becomes
    """
    package extractIfElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
          true
        else {
 /*(*/    test /*)*/
        }
      }

      private def test: Boolean = {
        false
      }
    }
    """
  } applyRefactoring extract("test")

  @Test
  def extractIfSingleLineElse = new FileSet {
    """
    package extractIfSingleLineElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true) true else /*(*/ false /*)*/
      }
    }
    """ becomes
    """
    package extractIfSingleLineElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true) true else /*(*/ test /*)*/
      }

      private def test: Boolean = {
        false
      }
    }
    """
  } applyRefactoring extract("test")

  @Test
  def extractIfElseTry = new FileSet {
    """
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
    """ becomes
    """
    package extractIfElseTry
    class A {
      def extractFrom(): Boolean = {
        if(true == true) test  /*)*/
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

      private def test: Boolean = {
         /*(*/  true
      }
    }
    """
  } applyRefactoring extract("test")

  @Test
  def extractCheckForFalse = new FileSet {
    """
          package extractCheckForFalse
      trait Check {
        def whatIsIt(check: Boolean) {
          if (/*(*/check == false/*)*/ /*hi*/)
            println("It's false")
          else
            println("It's true")
        }
      }
    """ becomes
    """
          package extractCheckForFalse
      trait Check {
        def whatIsIt(check: Boolean) {
          if (isFalse(check))
            println("It's false")
          else
            println("It's true")
        }

        private def isFalse(check: Boolean): Boolean = {
          /*(*/check == false/*)*/ /*hi*/
        }
      }
    """
  } applyRefactoring extract("isFalse")

  @Test
  def extractWithMethod = new FileSet {
    """
    package extractWithMethod
    class A {
      def extractFrom(): Boolean = {
        val invert: Boolean => Boolean = ! _
        val a = false
/*(*/   val b = invert(a)    /*)*/
        b
      }
    }
    """ becomes
    """
    package extractWithMethod
    class A {
      def extractFrom(): Boolean = {
        val invert: Boolean => Boolean = ! _
        val a = false
        val b = certainlyTrue(invert, a)
        b
      }

      private def certainlyTrue(invert: Boolean => Boolean, a: Boolean): Boolean = {
/*(*/   val b = invert(a)    /*)*/
        b
      }
    }
    """
  } applyRefactoring extract("certainlyTrue")

  @Test
  def extractAllAtOnce = new FileSet {
    """
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
  """ becomes
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

      private def magic(sumList: Seq[Int] => Int, prodList: Seq[Int] => Int, values: List[Int]): (Int, Int) = {
    /*(*/    val     sum = sumList(values)   // the sum
        val product = prodList(values) /*)*/ // the product
        (sum, product)
      }
    }
  """
  } applyRefactoring extract("magic")

  @Test
  def extractLarger = new FileSet {
    """
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
  """ becomes
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

      private def isFalse(check: Boolean): Boolean = {
        /*(*/check == false/*)*/ /*hi*/
      }
    }

    object c2 {
      def blabla {
        println("blabla")
      }
    }
  """
  } applyRefactoring extract("isFalse")

  @Test
  def singleIfInMethod = new FileSet {
    """
object ExtractMethod2 {
  def method {
    if(true) {
      /*(*/println("true")/*)*/
    }
  }
}
    """ becomes
    """
object ExtractMethod2 {
  def method {
    if(true) {
      /*(*/certainlyTrue/*)*/
    }
  }

  private def certainlyTrue: Unit =  {
      /*(*/println("true")
  }
}
    """
  } applyRefactoring extract("certainlyTrue")

  @Test
  def localFunctionAsParameter = new FileSet {
    """
object ExtractWithLocalFunction {
  def method {
    def add1(x: Int) = x + 1
    val i = 1
    /*(*/add1(i)/*)*/
    ()
  }
}
    """ becomes
    """
object ExtractWithLocalFunction {
  def method {
    def add1(x: Int) = x + 1
    val i = 1
    call(add1, i)
    ()
  }

  private def call(add1: Int => Int, i: Int): Int = {
    /*(*/add1(i)/*)*/
  }
}
    """
  } applyRefactoring extract("call")

  @Test
  def localFunctionAsParameter2 = new FileSet {
    """
object ExtractWithLocalFunction2 {
  def method {
    def add(x: Int, y: Int) = x + y
    val i = 1
    val j = 1
    /*(*/add(i, j)/*)*/
    ()
  }
}
    """ becomes
    """
object ExtractWithLocalFunction2 {
  def method {
    def add(x: Int, y: Int) = x + y
    val i = 1
    val j = 1
    call(add, i, j)
    ()
  }

  private def call(add: (Int, Int) => Int, i: Int, j: Int): Int = {
    /*(*/add(i, j)/*)*/
  }
}
    """
  } applyRefactoring extract("call")

  @Test
  def localFunctionAsParameter3 = new FileSet {
    """
object ExtractWithLocalFunction3 {
  def method {
    def one() = 1
    /*(*/one/*)*/
    ()
  }
}
    """ becomes
    """
object ExtractWithLocalFunction3 {
  def method {
    def one() = 1
    call(one)
    ()
  }

  private def call(one: () => Int): Int = {
    /*(*/one/*)*/
  }
}
    """
  } applyRefactoring extract("call")

  @Test
  def bug18 = new FileSet {
    """
object Bar {
  def foo {
    val bubu = "abc"
    bubu.format()
    /*(*/ 10 * 4 - 1 /*)*/
  }
}
""" becomes
    """
object Bar {
  def foo {
    val bubu = "abc"
    bubu.format()
    calc
  }

  private def calc: Unit = {
    /*(*/ 10 * 4 - 1 /*)*/
  }
}
"""
  } applyRefactoring extract("calc")

  @Test
  def simpleYield = new FileSet {
    """
package simpleExtract
class A {
  def extractFrom {
  for (i <- 0 to 100) yield {
      /*(*/val j = i * 2;
      j/*)*/
  }
  }
}
    """ becomes
    """
package simpleExtract
class A {
  def extractFrom {
  for (i <- 0 to 100) yield {
    call(i)
  }
  }

  private def call(i: Int): Int = {
      /*(*/val j = i * 2;
    j/*)*/
  }
}
    """
  } applyRefactoring extract("call")

  @Test
  def extractFromMethodWithObject = new FileSet {
    """
package simpleExtract
class PathSeparator {
  def main() {

    /*(*/5 -> 10/*)*/

    import java.io.{ File => F }

    object Whatever {
      val sep = F.pathSeparator
    }
  }
}
    """ becomes
    """
package simpleExtract
class PathSeparator {
  def main() {
    mkTuple

    import java.io.{ File => F }

    object Whatever {
      val sep = F.pathSeparator
    }
  }

  private def mkTuple: (Int, Int) = {

    /*(*/5 -> 10/*)*/
  }
}
    """
  } applyRefactoring extract("mkTuple")

  @Test
  def extractFromMethodWithMultipleAssignment = new FileSet {
    """
package simpleExtract
class PathSeparator {
  def main() {
    val (x, y) = {
      /*(*/println("hello")/*)*/
      (1,2)
    }
  }
}
    """ becomes
    """
package simpleExtract
class PathSeparator {
  def main() {
    val (x, y) = {
      sayHello
      (1,2)
    }
  }

  private def sayHello: Unit = {
      /*(*/println("hello")/*)*/
  }
}
    """
  } applyRefactoring extract("sayHello")

  @Test
  def extractFromWithinAnonymousClass = new FileSet {
    """
    class ExtractFromAnonClass {
      def method {
        val o= new Object {
          def f() {
            val i= /*(*/1 + 2/*)*/
          }
        }
      }
    }
    """ becomes
    """
    class ExtractFromAnonClass {
      def method {
        val o= new Object {
          def f() {
            val i= three/*)*/
          }

          private def three: Int = {
            /*(*/1 + 2
          }
        }
      }
    }
    """
  } applyRefactoring extract("three")

  @Test(expected=classOf[PreparationException])
  def extractionNeedsSelection = new FileSet {
    """
    class Foo {
      def bar = {
        List(1,2,3).mkString(/*(*//*)*/",")
      }
    }
    """ becomes
    ""
  } applyRefactoring extract("sep")
}
