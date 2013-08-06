package scala.tools.refactoring
package tests.implementations

import implementations.GenerateHashcodeAndEquals
import tests.util.TestHelper
import tests.util.TestRefactoring
import org.junit.Ignore
import org.junit.After
import scala.tools.refactoring.util.CompilerInstance

import language.reflectiveCalls

class GenerateHashcodeAndEqualsTest extends TestHelper with TestRefactoring {

  outer =>

  // We are experiencing instable test runs, maybe it helps when we
  // use a fresh compiler for each test case:

  override val global = (new CompilerInstance).compiler

  @After
  def shutdownCompiler {
    global.askShutdown
  }

  def generateHashcodeAndEquals(params: (Boolean, String => Boolean, Boolean))(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new GenerateHashcodeAndEquals with SilentTracing {
      val global = outer.global
    }
    import refactoring.global.ValDef
    val paramsFilter = (param: ValDef) => params._2(param.name.toString)
    val changes = performRefactoring(refactoring.RefactoringParameters(params._1, paramsFilter, params._3))
  }.changes

  @Test
  def singleValParam = new FileSet {
    """
      package generateHashcodeAndEquals.singleValParam

      class /*(*/Foo/*)*/(val param: String)
    """ becomes
      """
      package generateHashcodeAndEquals.singleValParam

      class /*(*/Foo/*)*/(val param: String) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.singleValParam.Foo]
        }

        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.singleValParam.Foo => that.canEqual(Foo.this) && param == that.param
            case _ => false
          }
        }

        override def hashCode() = {
          val prime = 41
          prime + param.hashCode
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, p => true, false)))

  @Test
  def twoValParams = new FileSet {
    """
      package generateHashcodeAndEquals.twoValParams

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int])
    """ becomes
      """
      package generateHashcodeAndEquals.twoValParams

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.twoValParams.Foo]
        }

        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.twoValParams.Foo => that.canEqual(Foo.this) && p1 == that.p1 && p2 == that.p2
            case _ => false
          }
        }

        override def hashCode() = {
          val prime = 41
          prime * (prime + p1.hashCode) + p2.hashCode
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, p => true, false)))

  @Test
  def excludeNonPublicParams = new FileSet {
    """
      package generateHashcodeAndEquals.excludeNonPublicParams

      class /*(*/Foo/*)*/(p1: String, val p2: List[Int])
    """ becomes
      """
      package generateHashcodeAndEquals.excludeNonPublicParams

      class /*(*/Foo/*)*/(p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.excludeNonPublicParams.Foo]
        }

        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.excludeNonPublicParams.Foo => that.canEqual(Foo.this) && p2 == that.p2
            case _ => false
          }
        }

        override def hashCode() = {
          val prime = 41
          prime + p2.hashCode
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, _ => true, false)))

  @Test
  def keepExistingEquals = new FileSet {
    """
      package generateHashcodeAndEquals.keepExistingEquals

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) {
        override def equals(other: Any) = false
      }
    """ becomes
    """
      package generateHashcodeAndEquals.keepExistingEquals

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) extends Equals {
        override def equals(other: Any) = false

        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.keepExistingEquals.Foo]
        }

        override def hashCode() = {
          val prime = 41
          prime * (prime + p1.hashCode) + p2.hashCode
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, _ => true, true)))

  @Test
  def dropExistingHashCode = new FileSet {
    """
      package generateHashcodeAndEquals.dropExistingHashCode

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) {
        override def hashCode() = 57
      }
    """ becomes
      """
      package generateHashcodeAndEquals.dropExistingHashCode

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.dropExistingHashCode.Foo]
        }

        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.dropExistingHashCode.Foo => that.canEqual(Foo.this) && p1 == that.p1 && p2 == that.p2
            case _ => false
          }
        }

        override def hashCode() = {
          val prime = 41
          prime * (prime + p1.hashCode) + p2.hashCode
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, _ => true, false)))

  @Test
  def keepExistingCanEqual = new FileSet {
    """
      package generateHashcodeAndEquals.keepExistingCanEqual

      class /*(*/Foo/*)*/ extends Equals {
        def canEqual(that: Any) = false
      }
    """ becomes
    """
      package generateHashcodeAndEquals.keepExistingCanEqual

      class /*(*/Foo/*)*/ extends Equals {
        def canEqual(that: Any) = false

        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.keepExistingCanEqual.Foo => that.canEqual(Foo.this)
            case _ => false
          }
        }

        override def hashCode() = {
          val prime = 41
          prime
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, _ => true, true)))

  @Test
  def selectByName = new FileSet {
    """
    package generateHashcodeAndEquals.selectByName

    class /*(*/Foo/*)*/(val p1: String, val p2: List[Int], var p3: Boolean, val p4: List[Boolean])
    """ becomes
      """
    package generateHashcodeAndEquals.selectByName

    class /*(*/Foo/*)*/(val p1: String, val p2: List[Int], var p3: Boolean, val p4: List[Boolean]) extends Equals {
      def canEqual(other: Any) = {
        other.isInstanceOf[generateHashcodeAndEquals.selectByName.Foo]
      }

      override def equals(other: Any) = {
        other match {
          case that: generateHashcodeAndEquals.selectByName.Foo => that.canEqual(Foo.this) && p2 == that.p2 && p3 == that.p3
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime * (prime + p2.hashCode) + p3.hashCode
      }
    }
    """
  } applyRefactoring (generateHashcodeAndEquals(false, (name: String) =>
      name match {
        case "p2" => true
        case "p3" => true
        case _ => false
      }
  , false))

  @Test
  def callSuper = new FileSet {
    """
      package generateHashcodeAndEquals.callSuper

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int])
    """ becomes
      """
      package generateHashcodeAndEquals.callSuper

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.callSuper.Foo]
        }

        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.callSuper.Foo => Foo.super.equals(that) && that.canEqual(Foo.this) && p1 == that.p1 && p2 == that.p2
            case _ => false
          }
        }

        override def hashCode() = {
          val prime = 41
          prime * (prime * Foo.super.hashCode() + p1.hashCode) + p2.hashCode
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((true, _ => true, false)))

  @Test
  def emptyClassBody = new FileSet {
    """
      package generateHashcodeAndEquals.emptyClassBody

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) {

      }
    """ becomes
      """
      package generateHashcodeAndEquals.emptyClassBody

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.emptyClassBody.Foo]
        }

        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.emptyClassBody.Foo => Foo.super.equals(that) && that.canEqual(Foo.this) && p1 == that.p1 && p2 == that.p2
            case _ => false
          }
        }

        override def hashCode() = {
          val prime = 41
          prime * (prime * Foo.super.hashCode() + p1.hashCode) + p2.hashCode
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((true, _ => true, true)))

  @Test
  def noParams = new FileSet {
    """
    package generateHashcodeAndEquals.noParams
    class /*(*/Foo/*)*/ {
    }
    """  becomes
    """
    package generateHashcodeAndEquals.noParams
    class /*(*/Foo/*)*/ extends Equals {
      def canEqual(other: Any) = {
        other.isInstanceOf[generateHashcodeAndEquals.noParams.Foo]
      }

      override def equals(other: Any) = {
        other match {
          case that: generateHashcodeAndEquals.noParams.Foo => that.canEqual(Foo.this)
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime
      }
    }
    """
  } applyRefactoring(generateHashcodeAndEquals((false, _ => false, false)))

  @Test(expected = classOf[PreparationException])
  def traitFails = new FileSet {
    """
    package generateHashcodeAndEquals.traitFails
    trait NotAClass
    """ becomes
    ""
  } applyRefactoring(generateHashcodeAndEquals((false, _ => false, false)))

  @Test
  def replaceHashcodeVal = new FileSet {
    """
    package generateHashcodeAndEquals.replaceHashcodeVal
    class /*(*/Foo/*)*/ {
      override val hashCode = 57
    }
    """ becomes
    """
    package generateHashcodeAndEquals.replaceHashcodeVal
    class /*(*/Foo/*)*/ extends Equals {
      def canEqual(other: Any) = {
        other.isInstanceOf[generateHashcodeAndEquals.replaceHashcodeVal.Foo]
      }

      override def equals(other: Any) = {
        other match {
          case that: generateHashcodeAndEquals.replaceHashcodeVal.Foo => that.canEqual(Foo.this)
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime
      }
    }
    """
  } applyRefactoring(generateHashcodeAndEquals((false, _ => false, false)))

  @Test
  def dontExtendEqualsTwice = new FileSet {
    """
    class /*(*/Foo/*)*/ extends Equals {
      def canEqual(that: Any) = false
    }
    """ becomes
    """
    class /*(*/Foo/*)*/ extends Equals {
      def canEqual(that: Any) = false

      override def equals(other: Any) = {
        other match {
          case that: Foo => that.canEqual(Foo.this)
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime
      }
    }
    """
  } applyRefactoring(generateHashcodeAndEquals((false, _ => false, true)))

  @Test
  def traitWithExistingSuperConstructorCall = new FileSet {
    """
    package traitWithExistingSuperConstructorCall

    class Point(val x: Int)
    class /*(*/ColoredPoint/*)*/(override val x: Int) extends Point(x)
    """ becomes
    """
    package traitWithExistingSuperConstructorCall

    class Point(val x: Int)
    class /*(*/ColoredPoint/*)*/(override val x: Int) extends Point(x) with Equals {
      def canEqual(other: Any) = {
        other.isInstanceOf[traitWithExistingSuperConstructorCall.ColoredPoint]
      }

      override def equals(other: Any) = {
        other match {
          case that: traitWithExistingSuperConstructorCall.ColoredPoint => that.canEqual(ColoredPoint.this)
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime
      }
    }
    """
  } applyRefactoring(generateHashcodeAndEquals((false, _ => false, false)))

}