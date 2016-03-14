/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.ExtractLocal
import tests.util.TestRefactoring
import tests.util.TestHelper


class ExtractLocalTest extends TestHelper with TestRefactoring {
  outer =>

  def extract(param: ExtractLocal#RefactoringParameters)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new ExtractLocal with TestProjectIndex
    }
    testRefactoring.performRefactoring(param)
  }

  @Test
  def extracPartOfChainedCalls() = new FileSet {
    """
      package extractLocal
      object Demo {
        def update(platform: String): Unit = {
          val x = new collection.mutable.ListBuffer[String]
     /*(*/x.toList/*)*/ mkString ","
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def update(platform: String): Unit = {
          val x = new collection.mutable.ListBuffer[String]
     /*(*/val asList = x.toList/*)*/ ▒
          asList mkString ","
        }
      }
    """
  } applyRefactoring(extract("asList"))

  @Test
  def extractIfCond() = new FileSet {
    """
      package extractLocal
      object Demo {
        def update(platform: String): Unit = {
          println("Update..")
          if(/*(*/platform.toUpperCase.indexOf("MAC") > -1/*)*/) {
            println("We're on a Mac!")
          }
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def update(platform: String): Unit = {
          println("Update..")
          val isMacOs = /*(*/platform.toUpperCase.indexOf("MAC") > -1/*)*/
          if(isMacOs) {
            println("We're on a Mac!")
          }
        }
      }
    """
  } applyRefactoring(extract("isMacOs"))

  @Test
  def extractLocal() = new FileSet {
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double): Unit = {

          val v = /*(*/3.14 * r * r/*)*/ * h

          println("volume is: "+ v)
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double): Unit = {
          val gr = 3.14 * r * r/*)*/ ▒

          val v = /*(*/gr * h

          println("volume is: "+ v)
        }
      }
    """
  } applyRefactoring(extract("gr"))

  @Test
  def extractFromElseWithoutParens() = new FileSet {
    """
      package extractLocal
      object Demo {
        def printSum(l: List[Int]): Unit = {

          println("Printing the sum..")

          if(l.isEmpty) {
            println("is empty :-(")
          } else
            println("sum is: "+  /*(*/l.reduceLeft(_ + _)/*)*/  )

          println(".. done")
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def printSum(l: List[Int]): Unit = {

          println("Printing the sum..")

          if(l.isEmpty) {
            println("is empty :-(")
          } else {
            val sum = l.reduceLeft(_ + _)
            println("sum is: "+  /*(*/sum/*)*/  )
          }

          println(".. done")
        }
      }
    """
  } applyRefactoring(extract("sum"))

  @Test
  def extractValRhs() = new FileSet {
    """
      object Demo {
        def update(platform: String): Unit = {
          val s = /*(*/platform/*)*/
        }
      }
    """ becomes
    """
      object Demo {
        def update(platform: String): Unit = {
          val plt = /*(*/platform
          val s = plt/*)*/
        }
      }
    """
  } applyRefactoring(extract("plt"))

  @Test
  def extractValRhs2() = new FileSet {
    """
      class Extr {
        def update(platform: String): Unit = {
          val s = ( /*(*/2*3/*)*/) + 1
        }
      }
    """ becomes
    """
      class Extr {
        def update(platform: String): Unit = {
          val six = 2*3/*)*/
          val s = six + 1
        }
      }
    """
  } applyRefactoring(extract("six"))

  @Test
  def extractFilter() = new FileSet {
    """
      class Extr2 {
        def m: Unit = {
          val list = (1 to 10) toList

     /*(*/list filter (_ > 3)/*)*/filter (_ < 6)
        }
      }
    """ becomes
    """
      class Extr2 {
        def m: Unit = {
          val list = (1 to 10) toList

     /*(*/val largerThree = list filter (_ > 3)/*)*/
          largerThree filter (_ < 6)
        }
      }
    """
  } applyRefactoring(extract("largerThree"))

  @Test
  def extractPartOfACondition() = new FileSet {
    """
      class Extr2 {
        def m: Unit = {
          val someValue = true
          if(someValue && /*(*/"aa".matches("\\w+")/*)*/) {
            println("yay")
          }
        }
      }
    """ becomes
    """
      class Extr2 {
        def m: Unit = {
          val someValue = true
          val part2 = "aa".matches("\\w+")
          if(someValue && /*(*/part2/*)*/) {
            println("yay")
          }
        }
      }
    """
  } applyRefactoring(extract("part2"))

  @Test
  def extractFromCaseWithMultipleStatements() = new FileSet {
    """
      class Extr2 {
        Nil match {
          case Nil =>
            val a = 5
            val b = /*(*/a + 1/*)*/
            b
        }
      }
    """ becomes
    """
      class Extr2 {
        Nil match {
          case Nil =>
            val a = 5
            val six = /*(*/a + 1
            val b = six/*)*/
            b
        }
      }
    """
  } applyRefactoring(extract("six"))

  @Test
  def extractFromCaseWithSingleStatement() = new FileSet {
    """
      class Extr2 {
        Nil match {
          case Nil =>
            /*(*/5 + 1/*)*/ toString
        }
      }
    """ becomes
    """
      class Extr2 {
        Nil match {
          case Nil =>
            val six = 5 + 1/*)*/ ▒
            /*(*/six toString
        }
      }
    """
  } applyRefactoring(extract("six"))

  @Test
  def extractFromCaseWithTwoStatements() = new FileSet {
    """
      class Extr2 {
        /*(*/5 + 1/*)*/toString
      }
    """ becomes
    """
      class Extr2 {
        val six = 5 + 1/*)*/

        /*(*/six toString
      }
    """
  } applyRefactoring(extract("six"))

  @Test
  def extractFromTry() = new FileSet {
    """
      class Extr2 {
        try {
          val a = List(1,2,3)
          /*(*/a filter (_> 2)/*)*/ mkString ", "
        }
      }
    """ becomes
    """
      class Extr2 {
        try {
          val a = List(1,2,3)
          val largerThanTwo = a filter (_> 2)/*)*/ ▒
          /*(*/largerThanTwo mkString ", "
        }
      }
    """
  } applyRefactoring(extract("largerThanTwo"))

  @Test
  def extractFromTrySingleStatement() = new FileSet {
    """
      class Extr2 {
        try {
          /*(*/List(1,2,3) filter (_> 2)/*)*/ mkString ", "
        }
      }
    """ becomes
    """
      class Extr2 {
        try {
          val largerThanTwo = List(1,2,3) filter (_> 2)/*)*/ ▒
          /*(*/largerThanTwo mkString ", "
        }
      }
    """
  } applyRefactoring(extract("largerThanTwo"))

  @Test
  def extractMethod() = new FileSet {
    """
      class Extr2 {
        /*(*/List(1,2,3) filter/*)*/ (_> 2) mkString ", "
      }
    """ becomes
    """
      class Extr2 {
        val filterList = List(1,2,3) filter/*)*/ _

        /*(*/filterList(_> 2) mkString ", "
      }
    """
  } applyRefactoring(extract("filterList"))

  @Test
  def extractFromFunctionWithCurlyBraces() = new FileSet {
    """
      class Extr2 {
        List(1,2,3) filter { it =>
          /*(*/it + 1 % 2/*)*/ == 0
        }
      }
    """ becomes
    """
      class Extr2 {
        List(1,2,3) filter { it =>
          val isOdd = it + 1 % 2/*)*/ ▒
          /*(*/isOdd == 0
        }
      }
    """
  } applyRefactoring(extract("isOdd"))

  @Test
  def extractFromFunction() = new FileSet {
    """
      class Extr2 {
        List(1,2,3) filter (i => /*(*/i + 1 % 2/*)*/ == 0)
      }
    """ becomes
    """
      class Extr2 {
        List(1,2,3) filter (i => {
          val isOdd = i + 1 % 2/*)*/ ▒
          /*(*/isOdd == 0
        })
      }
    """
  } applyRefactoring(extract("isOdd"))

  @Test
  def extractFromValBlock() = new FileSet {
      """
      class Extr2 {
        val a = {
          val i = 1
          /*(*/i + 2/*)*/
        }
      }
    """ becomes """
      class Extr2 {
        val a = {
          val i = 1
          val addTwo = /*(*/i + 2/*)*/
          addTwo
        }
      }
    """
  } applyRefactoring extract("addTwo")

  @Test
  def extractFromThen() = new FileSet {
    """
      class Extr2 {
        if(true) {
          /*(*/"a" + "b"/*)*/ + "c"
        }
      }
    """ becomes
    """
      class Extr2 {
        if(true) {
          val ab = "a" + "b"/*)*/ ▒
          /*(*/ab + "c"
        }
      }
    """
  } applyRefactoring(extract("ab"))

  @Test
  def extractFromThenWithoutParent() = new FileSet {
    """
      class Extr2 {
        if(true)
          /*(*/"a" + "b"/*)*/ + "c"
      }
    """ becomes
    """
      class Extr2 {
        if(true) {
          val ab = "a" + "b"/*)*/ ▒
          /*(*/ab + "c"
        }
      }
    """
  } applyRefactoring(extract("ab"))

  @Test
  def extractFromElse() = new FileSet {
    """

    object ExtractLocal1 {

      def main(args: Array[String]): Unit = {

        println("Detecting OS..")

        if(System.getProperties.get("os.name") == "Linux") {
          println("We're on Linux!")
        } else {
          println(/*(*/"We're not on Linux!"/*)*/)
        }

        println("Done.")
      }
    }
    """ becomes
    """

    object ExtractLocal1 {

      def main(args: Array[String]): Unit = {

        println("Detecting OS..")

        if(System.getProperties.get("os.name") == "Linux") {
          println("We're on Linux!")
        } else {
          val msg = /*(*/"We're not on Linux!"
          println(msg/*)*/)
        }

        println("Done.")
      }
    }
    """
  } applyRefactoring(extract("msg"))

  @Test
  def extractFromSimpleMethod() = new FileSet {
    """
      class Extr2 {
        def method: Unit = {
          println(/*(*/"Hello World"/*)*/)
        }
      }
    """ becomes
    """
      class Extr2 {
        def method: Unit = {
          val ab = /*(*/"Hello World"
          println(ab/*)*/)
        }
      }
    """
  } applyRefactoring(extract("ab"))

  @Test
  def extractFromMethod() = new FileSet {
    """
      class Extr2 {
        def method: Unit = {
          println(/*(*/"Hello World"/*)*/)
          println("Hello World!")
        }
      }
    """ becomes
    """
      class Extr2 {
        def method: Unit = {
          val ab = /*(*/"Hello World"
          println(ab/*)*/)
          println("Hello World!")
        }
      }
    """
  } applyRefactoring(extract("ab"))

  @Test
  def extractFromMethod2() = new FileSet {
    """
object ExtractMethod2 {

  def main(args: Array[String]): Unit = {

    val a = 1
    val b = /*(*/1/*)*/ //a
      val c=1 //comment
    val d = b + c
    println(a+b+c+d)
  }
}
    """ becomes
    """
object ExtractMethod2 {

  def main(args: Array[String]): Unit = {

    val a = 1
    val ab = /*(*/1
    val b = ab/*)*/ //a
      val c=1 //comment
    val d = b + c
    println(a+b+c+d)
  }
}
    """
  } applyRefactoring(extract("ab"))

  @Test
  @Ignore
  def extractFromFunction2() = new FileSet {
    """
      class Extr2 {
        List(1,2,3) filter (/*(*/_ + 1 % 2/*)*/ == 0)
      }
    """ becomes
    """"""
  } applyRefactoring(extract("isOdd"))

  @Test
  def extractFromCaseClause() = new FileSet {
    """
      class Extr2 {
        List() match {
          case Nil => true && /*(*/false/*)*/
        }
      }
    """ becomes
    """
      class Extr2 {
        List() match {
          case Nil => ▒
            val isFalse = false
            true && /*(*/isFalse/*)*/
        }
      }
    """
  } applyRefactoring(extract("isFalse"))

  @Test
  def extractFromMatch() = new FileSet {
    """
      class Extr2 {
        /*(*/List(1,2,3)/*)*/ match {
          case Nil => true
        }
      }
    """ becomes
    """
      class Extr2 {
        val l = List(1,2,3)/*)*/

        /*(*/l match {
          case Nil => true
        }
      }
    """
  } applyRefactoring(extract("l"))

  @Test
  def extractFunctionFromMethodCall() = new FileSet {
    """
      class Extr2 {
        def main(args : Array[String]): Unit = {
          def concatenate(ls: List[String]) = ls.mkString(", ")

          println(/*(*/concatenate/*)*/(List("a", "b")))
        }
      }
    """ becomes
    """
      class Extr2 {
        def main(args : Array[String]): Unit = {
          def concatenate(ls: List[String]) = ls.mkString(", ")
          val cc = concatenate/*)*/_

          println(/*(*/cc(List("a", "b")))
        }
      }
    """
  } applyRefactoring(extract("cc"))

  @Test
  def extractLastExpressionInUnitMethod() = new FileSet {
    """
    object Bar {
      def foo: Unit = {
        /*(*/true/*)*/
      }
    }""" becomes
    """
    object Bar {
      def foo: Unit = {
        /*(*/  val t = true
               t/*)*/
      }
    }"""
  } applyRefactoring(extract("t"))

  @Test
  def extractFromUpdateMethod() = new FileSet {
    """
    object ExtractLocalBugTest extends App{
      val List(one, three, eight) = List(1,3,8);
      printf("%d %d %d\n", one, three, eight)

      val strings = Array("One", "Second")
      strings(1) = /*(*/"Two"/*)*/
      for( str <- strings) println(str)
    }
    """ becomes
    """
    object ExtractLocalBugTest extends App {
      val List(one, three, eight) = List(1,3,8);
      printf("%d %d %d\n", one, three, eight)

      val strings = Array("One", "Second")

      val two = /*(*/"Two"
      strings(1) = two/*)*/
      for( str <- strings) println(str)
    }
    """
  } applyRefactoring(extract("two"))

  @Test
  def extractWithoutSelection() = new FileSet {
    """
    object ExtractLocal {
      def x(): Unit = {
        1/*(*//*)*/ + 1
        val test = "hello"
      }
    }
    """ becomes
    """
    object ExtractLocal {
      def x(): Unit = {
        val one = 1
        1/*(*//*)*/ + one
        val test = "hello"
      }
    }
    """
  } applyRefactoring(extract("one"))

  @Test
  def extractFromFor() = new FileSet {
    """
    object ExtractFromFor {

      val l = List(1,2)

      for (i <- 0 until /*(*/l.length/*)*/) yield i
    }
    """ becomes
    """
    object ExtractFromFor {

      val l = List(1,2)

      val len = l.length

      for (i <- 0 until /*(*/len) yield i
    }
    """
  } applyRefactoring(extract("len"))

  @Test
  def extractFromForFilter() = new FileSet {
    """
    object ExtractFromFor {

      val l = List(1,2)

      for (i <- List(1,2) if i == /*(*/"abc".length/*)*/) yield i
    }
    """ becomes
    """
    object ExtractFromFor {

      val l = List(1,2)

      val len = "abc".length

      for (i <- List(1,2) if i == /*(*/len) yield i
    }
    """
  } applyRefactoring(extract("len"))

  @Test
  def extractFromForFilterYieldWithBody() = new FileSet {
    """
    object ExtractFromFor {

      val l = List(1,2)

      for (i <- List(1,2) if i == /*(*/"abc".length/*)*/) yield {
        i * i
      }
    }
    """ becomes
    """
    object ExtractFromFor {

      val l = List(1,2)

      val len = "abc".length

      for (i <- List(1,2) if i == /*(*/len) yield {
        i * i
      }
    }
    """
  } applyRefactoring(extract("len"))

  @Test
  def extractFromForFilterYieldWithSameLineBody() = new FileSet {
    """
    object ExtractFromFor {

      val l = List(1,2)

      for (i <- List(1,2) if i == /*(*/"abc".length/*)*/) yield { i * i }
    }
    """ becomes
    """
    object ExtractFromFor {

      val l = List(1,2)

      val len = "abc".length

      for (i <- List(1,2) if i == /*(*/len) yield { i * i }
    }
    """
  } applyRefactoring(extract("len"))

  @Test
  def extractFromForFilterYieldWithBlockBody() = new FileSet {
    """
    object ExtractFromFor {

      val l = List(1,2)

      for (i <- List(1,2) if i == /*(*/"abc".length/*)*/) yield {
        val m = 2
        i * 2
      }
    }
    """ becomes
    """
    object ExtractFromFor {

      val l = List(1,2)

      val len = "abc".length

      for (i <- List(1,2) if i == /*(*/len) yield {
        val m = 2
        i * 2
      }
    }
    """
  } applyRefactoring(extract("len"))

  @Test
  def extractAnonFunction() = new FileSet {
    """
    object ExtractFromFor {
      val inc = /*(*/(_:Int)+1/*)*/
    }
    """ becomes
    """
    object ExtractFromFor {
      val plusOne: Int => Int = /*(*/(_:Int)+1

      val inc = plusOne/*)*/
    }
    """
  } applyRefactoring(extract("plusOne"))

 @Test
 def extractList() = new FileSet {
   """
   class ExtractList {
     val list = /*(*/1::Nil/*)*/
   }
   """ becomes
   """
   class ExtractList {
     val extracted = /*(*/1::Nil

     val list = extracted/*)*/
   }
   """
 } applyRefactoring(extract("extracted"))

 @Test
 def extractFromConstructor() = new FileSet {
   """
   object ExtractFromHere {
     import java.net.URL

     val pluginXmlUrl = {
       new URL(/*(*/""/*)*/)
     }
   }
   """ becomes
   """
   object ExtractFromHere {
     import java.net.URL

     val url = /*(*/""

     val pluginXmlUrl = {
       new URL(url/*)*/)
     }
   }
   """
 } applyRefactoring(extract("url"))

  @Test
  def plusAssignRhs() = new FileSet {
   """
    class ExtractLocalVariable {
      def f(): Unit = {
        var i= 3
        i += /*(*/g()/*)*/ // select 'g()' here
      }
      def g()= 2
    }
   """ becomes
   """
    class ExtractLocalVariable {
      def f(): Unit = {
        var i= 3
        val result_of_g = g()
        i += /*(*/result_of_g/*)*/ // select 'g()' here
      }
      def g()= 2
    }
   """
  } applyRefactoring(extract("result_of_g"))

  @Test
  def fromForExpressionReturningUnit() = new FileSet {
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        for (x <- /*(*/List(1,2,3,4)/*)*/) println(x)
      }
    }
   """ becomes
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        val lst = List(1,2,3,4)
        for (x <- /*(*/lst) println(x)
      }
    }
   """
  } applyRefactoring(extract("lst"))

  @Test
  def fromForExpression() = new FileSet {
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        for (x <- /*(*/List(1,2,3,4)/*)*/) yield x
      }
    }
   """ becomes
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        val lst = List(1,2,3,4)
        for (x <- /*(*/lst) yield x
      }
    }
   """
  } applyRefactoring(extract("lst"))

  @Test
  def fromForExpressionNoEnclosingBlock() = new FileSet {
   """
    class ExtractfromForExpression {
      def f() = for (x <- /*(*/List(1,2,3,4)/*)*/) yield x
    }
   """ becomes
   """
    class ExtractfromForExpression {
      def f() = {
        val lst = List(1,2,3,4)
        for (x <- /*(*/lst) yield x
      }
    }
   """
  } applyRefactoring(extract("lst"))

  @Test
  def fromForExpressionCurlyOneLiner() = new FileSet {
   """
    class ExtractfromForExpression {
      def f() = {
        for { x <- /*(*/List(1,2,3,4)/*)*/} yield x
      }
    }
   """ becomes
   """
    class ExtractfromForExpression {
      def f() = {
        val lst = List(1,2,3,4)
        for { x <- /*(*/lst} yield x
      }
    }
   """
  } applyRefactoring(extract("lst"))

  @Test
  def fromForExpressionWithFilter() = new FileSet {
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        for (x <- /*(*/List(1,2,3,4)/*)*/ if x == 2) yield {
          "found it"
        }
      }
    }
   """ becomes
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        val lst = List(1,2,3,4)
        for (x <- /*(*/lst if x == 2) yield {
          "found it"
        }
      }
    }
   """
  } applyRefactoring(extract("lst"))

  @Test
  def fromForExpressionCurly() = new FileSet {
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        for {
          x <- /*(*/List(1,2,3,4)/*)*/
        } yield {
          "found it"
        }
      }
    }
   """ becomes
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        val lst = List(1,2,3,4)
        for {
          x <- /*(*/lst
        } yield {
          "found it"
        }
      }
    }
   """
  } applyRefactoring(extract("lst"))

  /* It would be nice to get this to work, but without
   * proper support for for-expressions in the AST it's
   * going to be very difficult. */
  @Test
  @Ignore
  def fromForExpressionMultiple() = new FileSet {
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        for {
          x <- /*(*/List(1,2,3,4)/*)*/
          y <- List(1,2,3,4)
        } yield {
          x + y
        }
      }
    }
   """ becomes
   """"""
  } applyRefactoring(extract("lst"))

  @Test
  def fromForBlockBody() = new FileSet {
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        for {
          y <- List(1,2,3,4)
        } yield {
          println("this is the body")
          /*(*/y * 3/*)*/
        }
      }
    }
   """ becomes
   """
    class ExtractfromForExpression {
      def f(): Unit = {
        for {
          y <- List(1,2,3,4)
        } yield {
          println("this is the body")
          val lst = /*(*/y * 3/*)*/
          lst
        }
      }
    }
   """
  } applyRefactoring(extract("lst"))

  @Test
  def extractFromForEntireRhs() = new FileSet {
    """
    object ExtractFromFor {
      def foo: Unit = {
        for (i <- /*(*/List()/*)*/) println(i)
      }
    }
    """ becomes
    """
    object ExtractFromFor {
      def foo: Unit = {
        val extractedValue = List()
        for (i <- /*(*/extractedValue) println(i)
      }
    }
    """
  } applyRefactoring(extract("extractedValue"))

  @Test
  def extractFromForEntireRhsYield() = new FileSet {
    """
    object ExtractFromFor {
      def foo: Unit = {
        println(for (i <- /*(*/List()/*)*/) yield i)
      }
    }
    """ becomes
    """
    object ExtractFromFor {
      def foo: Unit = {
        val extractedValue = List()
        println(for (i <- /*(*/extractedValue) yield i)
      }
    }
    """
  } applyRefactoring(extract("extractedValue"))

  @Test
  def missingTypeNeedsAnnotation() = new FileSet {
    """
    object ExtractmissingTypeNeedsAnnotation {
      def foo: Unit = {
        Nil.filter(/*(*/{ case _ => true }/*)*/)
      }
    }
    """ becomes
    """
    object ExtractmissingTypeNeedsAnnotation {
      def foo: Unit = {
        val pred: Nothing => Boolean = /*(*/{ case _ => true }
        Nil.filter(pred/*)*/)
      }
    }
    """
  } applyRefactoring(extract("pred"))

  @Test
  def extractAnonFunctionWithoutTypeAscriptions() = new FileSet {
    """
    object ExtractFromFor {
      def foo: Unit = {
        List(1, 2, 3).reduceLeft(/*(*/_ + _/*)*/)
      }
    }
    """ becomes
    """
    object ExtractFromFor {
      def foo: Unit = {
        val sum: (Int, Int) => Int = /*(*/_ + _
        List(1, 2, 3).reduceLeft(sum/*)*/)
      }
    }
    """
  } applyRefactoring(extract("sum"))

  @Test
  def extractAnonFunctionWithoutTypeAscription() = new FileSet {
    """
    object ExtractFromFor {
      def foo: Unit = {
        List(1, 2, 3).map(/*(*/_ + 1/*)*/)
      }
    }
    """ becomes
    """
    object ExtractFromFor {
      def foo: Unit = {
        val addOne: Int => Int = /*(*/_ + 1
        List(1, 2, 3).map(addOne/*)*/)
      }
    }
    """
  } applyRefactoring(extract("addOne"))

  @Test
  def extractMatchBodyOperatorMap() = new FileSet {
    """
    object ExtractFromFor {
      def foo: Unit = {
        List(1, 2, 3) map /*(*/{
          case x => 1
        }/*)*/
      }
    }
    """ becomes
    """
    object ExtractFromFor {
      def foo: Unit = {
        val toOne: Int => Int = {
          case x => 1
        }
        List(1, 2, 3) map /*(*/toOne/*)*/
      }
    }
    """
  } applyRefactoring(extract("toOne"))

  @Test
  def extractMatchBody() = new FileSet {
    """
    object ExtractFromFor {
      def foo: Unit = {
        List(1, 2, 3).map /*(*/{
          case x => 1
        }/*)*/
      }
    }
    """ becomes
    """
    object ExtractFromFor {
      def foo: Unit = {
        val toOne: Int => Int = {
          case x => 1
        }
        List(1, 2, 3).map /*(*/(toOne)/*)*/
      }
    }
    """
  } applyRefactoring(extract("toOne"))
}
