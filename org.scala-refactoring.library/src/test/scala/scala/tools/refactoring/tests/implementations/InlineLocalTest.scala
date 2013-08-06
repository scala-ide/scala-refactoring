/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.InlineLocal
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._

import language.reflectiveCalls

class InlineLocalTest extends TestHelper with TestRefactoring {
  outer =>

  def inline(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new InlineLocal with SilentTracing with TestProjectIndex
    val changes = performRefactoring(new refactoring.RefactoringParameters)
  }.changes

  @Test
  def inlineIfCond = new FileSet {
    """
      package extractLocal
      object Demo {
        def update(platform: String) {
          println("Update..")
          /*(*/val isMacOs = platform.toUpperCase.indexOf("MAC") > -1/*)*/
          if(isMacOs) {
            println("We're on a Mac!")
          }
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def update(platform: String) {
          println("Update..")
          if(platform.toUpperCase.indexOf("MAC") > -1) {
            println("We're on a Mac!")
          }
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inlineInMatch = new FileSet {
    """
  object ExtractLocal1 {

    def main(args: Array[String]) {

      args toList match {
        case x :: Nil =>
          val x = /*(*/"one argument"/*)*/
          println(x)
        case _ =>
          println("more than one argument")
      }
    }
  }""" becomes
    """
  object ExtractLocal1 {

    def main(args: Array[String]) {

      args toList match {
        case x :: Nil =>
          println(/*(*/"one argument")
        case _ =>
          println("more than one argument")
      }
    }
  }"""
  } applyRefactoring(inline)

  @Test
  def inlineLocal = new FileSet {
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double) {
           /*(*/val gr = 3.14 * r * r/*)*/

          val v = gr * h

          println("volume is: "+ v)
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double) {

          val v = 3.14 * r * r* h

          println("volume is: "+ v)
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromElseWithoutParens = new FileSet {
    """
      package extractLocal
      object Demo {
        def printSum(l: List[Int]) {

          println("Printing the sum..")

          if(l.isEmpty) {
            println("is empty :-(")
          } else {
            /*(*/val sum = l.reduceLeft(_ + _)/*)*/
            println("sum is: "+ sum)
          }

          println(".. done")
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def printSum(l: List[Int]) {

          println("Printing the sum..")

          if(l.isEmpty) {
            println("is empty :-(")
          } else {
            println("sum is: "+ l.reduceLeft(_ + _))
          }

          println(".. done")
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inlineValRhs = new FileSet {
    """
      object Demo {
        def update(platform: String) {
          /*(*/val plt = platform/*)*/
          val s = plt
          val t = plt
          val u = plt
          val v = plt
        }
      }
    """ becomes
    """
      object Demo {
        def update(platform: String) {
          val s = platform
          val t = platform
          val u = platform
          val v = platform
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inlineFilter = new FileSet {
    """
      class Extr2 {
        def m {
          val list = (1 to 10) toList

     /*(*/val largerThree = list filter (_ > 3)/*)*/
          largerThree filter (_ < 6)
        }
      }
    """ becomes
    """
      class Extr2 {
        def m {
          val list = (1 to 10) toList
          list filter (_ > 3)filter (_ < 6)
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inlineFilterFunction = new FileSet {
    """
      class Extr2 {
        def m {
          val list = (1 to 10) toList

     /*(*/val largerThree = list filter _/*)*/
          largerThree (_ > 3)
        }
      }
    """ becomes
    """
      class Extr2 {
        def m {
          val list = (1 to 10) toList
          list filter  (_ > 3)
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inlinePartOfACondition = new FileSet {
    """
      class Extr2 {
        def m {
          val someValue = true
          /*(*/val part2 = "aa".matches("\\w+")/*)*/
          if(someValue && part2) {
            println("yay")
          }
        }
      }
    """ becomes
    """
      class Extr2 {
        def m {
          val someValue = true
          if(someValue && "aa".matches("\\w+")) {
            println("yay")
          }
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromCaseWithMultipleStatements = new FileSet {
    """
      class Extr2 { def m {
        Nil match {
          case Nil =>
            val a = 5
            val six = /*(*/a + 1/*)*/
            val b = six
            b
        }
      }}
    """ becomes
    """
      class Extr2 { def m {
        Nil match {
          case Nil =>
            val a = 5
            val b = /*(*/a + 1
            b
        }
      }}
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromCaseWithSingleStatement = new FileSet {
    """
      class Extr2 { def m {
        Nil match {
          case Nil =>
            println("huhu")
            /*(*/val six = 5 + 1/*)*/
            six
        }
      }}
    """ becomes
    """
      class Extr2 { def m {
        Nil match {
          case Nil =>
            println("huhu")
            5 + 1
        }
      }}
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromCaseWithTwoStatements = new FileSet {
    """
      class Extr2 { def m {
        /*(*/val six = 5 + 1/*)*/
        six toString
      }}
    """ becomes
    """
      class Extr2 { def m {
        5 + 1 toString
      }}
    """
  } applyRefactoring(inline)

  @Test
  def inliningNeedsParens = new FileSet {
    """
      class Extr2 {
        def m {
          /*(*/val five = 5 toString/*)*/;
          println(five)
          five + "a"
        }
      }
    """ becomes
    """
      class Extr2 {
        def m {
          println(5 toString)
          (5 toString) + "a"
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inliningNeedsParens2 = new FileSet {
    """
      class Extr2 {
        def m {
          /*(*/val largerThree = List(1) filter (_ > 3)/*)*/;
          println(largerThree.size)
          largerThree.size + "a"
        }
      }
    """ becomes
    """
      class Extr2 {
        def m {
          println((List(1) filter (_ > 3)).size)
          (List(1) filter (_ > 3)).size + "a"
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inliningNeedsNoParens = new FileSet {
    """
      class Extr2 {
        def m {
          /*(*/val largerThree /*)*/= (List(1) filter (_ > 3))
          println(largerThree.size)
          largerThree.size + "a"
        }
      }
    """ becomes
    """
      class Extr2 {
        def m {
          println((List(1) filter (_ > 3)).size)
          (List(1) filter (_ > 3)).size + "a"
        }
      }
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromTry = new FileSet {
    """
      class Extr2 { def m {
        try {
          val a = List(1,2,3)
           /*(*/val largerThanTwo = a filter (_> 2)/*)*/
          largerThanTwo mkString ", "
        }
      }}
    """ becomes
    """
      class Extr2 { def m {
        try {
          val a = List(1,2,3)
          a filter (_> 2)mkString ", "
        }
      }}
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromFunctionWithCurlyBraces = new FileSet {
    """
      class Extr2 { def m {
        List(1,2,3) filter { it =>
          /*(*/val isOdd = it + 1 % 2/*)*/
          isOdd == 0
        }
      }}
    """ becomes
    """
      class Extr2 { def m {
        List(1,2,3) filter { it =>
          /*(*/
          it + 1 % 2== 0
        }
      }}
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromValBlock = new FileSet {
    """
      class Extr2 { def m {
        val a = {
          val i = 1
          val addTwo = /*(*/i + 2/*)*/
          addTwo
        }
      }}
    """ becomes
    """
      class Extr2 { def m {
        val a = {
          val i = 1
          /*(*/i + 2
        }
      }}
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromThen = new FileSet {
    """
      class Extr2 { def m {
        if(true) {
          /*(*/val ab = "a" + "b"/*)*/
          ab + "c"
        }
      }}
    """ becomes
    """
      class Extr2 { def m {
        if(true) {
          "a" + "b"+ "c"
        }
      }}
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromElse = new FileSet {
    """

    object ExtractLocal1 {

      def main(args: Array[String]) {

        println("Detecting OS..")

        if(System.getProperties.get("os.name") == "Linux") {
          println("We're on Linux!")
        } else {
          val msg = /*(*/"We're not on Linux!"/*)*/
          println(msg)
        }

        println("Done.")
      }
    }
    """ becomes
    """

    object ExtractLocal1 {

      def main(args: Array[String]) {

        println("Detecting OS..")

        if(System.getProperties.get("os.name") == "Linux") {
          println("We're on Linux!")
        } else {
          println(/*(*/"We're not on Linux!")
        }

        println("Done.")
      }
    }
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromSeveralVals = new FileSet {
    """
    class InlineTest {
      def m {
        val /*(*//*)*/bbb = 42
        val c = List(bbb)
        c
      }
    }
    """ becomes
    """
    class InlineTest {
      def m {
        val c = List(42)
        c
      }
    }
    """
  } applyRefactoring(inline)

  @Test
  def inlineFromVal = new FileSet {
    """
    class InlineTest {
      def m {
        val bbb = 42
        val c = List(/*(*/bbb/*)*/)
        c
      }
    }
    """ becomes
    """
    class InlineTest {
      def m {
        val c = List(42/*)*/)
        c
      }
    }
    """
  } applyRefactoring(inline)

  @Test
  def inlineInListConcatenation = new FileSet {
    """
     class InlineTest {
       def m {
         val /*(*/as/*)*/ = List(1, 2)
         val concatenated = as:::List(3, 4)
       }
     }
    """ becomes
    """
     class InlineTest {
       def m {
         val concatenated = List(1, 2):::List(3, 4)
       }
     }
    """
  } applyRefactoring(inline)
}
