package scala.tools.refactoring
package tests.implementations

import implementations.IntroduceProductNTrait
import tests.util.TestHelper
import tests.util.TestRefactoring
import scala.tools.refactoring.implementations.IntroduceProductNTrait
import language.reflectiveCalls
import org.junit.After
import scala.tools.refactoring.util.CompilerInstance

class IntroduceProductNTraitTest extends TestHelper with TestRefactoring {

  outer =>

  // We are experiencing instable test runs, maybe it helps when we
  // use a fresh compiler for each test case:

  override val global = (new CompilerInstance).compiler

  @After
  def shutdownCompiler {
    global.askShutdown
  }

  def introduceProductNTrait(params: (Boolean, String => Boolean, Boolean))(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new IntroduceProductNTrait with SilentTracing {
      val global = outer.global
    }
    import refactoring.global.ValDef
    val paramsFilter = (param: ValDef) => params._2(param.name.toString)
    val changes = performRefactoring(refactoring.RefactoringParameters(params._1, paramsFilter, params._3))
  }.changes

  @Test
  def product1Simple = new FileSet {
    """
    package introduceProductNTrait.product1Simple

    class /*(*/Foo/*)*/(val param: String)
    """ becomes
    """
    package introduceProductNTrait.product1Simple

    class /*(*/Foo/*)*/(val param: String) extends Product1[String] {
      def _1() = {
        param
      }

      def canEqual(other: Any) = {
        other.isInstanceOf[introduceProductNTrait.product1Simple.Foo]
      }

      override def equals(other: Any) = {
        other match {
          case that: introduceProductNTrait.product1Simple.Foo => that.canEqual(Foo.this) && param == that.param
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime + param.hashCode
      }
    }
    """
  } applyRefactoring(introduceProductNTrait(false, _ => true, false))

  @Test
  def product2Simple = new FileSet {
    """
    package introduceProductNTrait.product2Simple

    class /*(*/Foo/*)*/(val p1: String, val p2: Int)
    """ becomes
    """
    package introduceProductNTrait.product2Simple

    class /*(*/Foo/*)*/(val p1: String, val p2: Int) extends Product2[String, Int] {
      def _1() = {
        p1
      }

      def _2() = {
        p2
      }

      def canEqual(other: Any) = {
        other.isInstanceOf[introduceProductNTrait.product2Simple.Foo]
      }

      override def equals(other: Any) = {
        other match {
          case that: introduceProductNTrait.product2Simple.Foo => that.canEqual(Foo.this) && p1 == that.p1 && p2 == that.p2
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime * (prime + p1.hashCode) + p2.hashCode
      }
    }
    """
  } applyRefactoring(introduceProductNTrait(false, _ => true, false))

  @Test
  def multipleTraits = new FileSet {
    """
    package introduceProductNTrait.multipleTraits

    class /*(*/Foo/*)*/(var p: Int) extends Serializable
    """ becomes
    """
    package introduceProductNTrait.multipleTraits

    class /*(*/Foo/*)*/(var p: Int) extends Serializable with Product1[Int] {
      def _1() = {
        p
      }

      def canEqual(other: Any) = {
        other.isInstanceOf[introduceProductNTrait.multipleTraits.Foo]
      }

      override def equals(other: Any) = {
        other match {
          case that: introduceProductNTrait.multipleTraits.Foo => Foo.super.equals(that) && that.canEqual(Foo.this) && p == that.p
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime * Foo.super.hashCode() + p.hashCode
      }
    }
    """
  } applyRefactoring(introduceProductNTrait(true, s => s == "p", false))

  @Test
  def nonPublicClassParams = new FileSet {
    """
    package introduceProductNTrait.nonPublicClassParams

    class /*(*/Foo/*)*/(val immutable: Int, var mutable: Int, nonpublic: Int)
    """ becomes
    """
    package introduceProductNTrait.nonPublicClassParams

    class /*(*/Foo/*)*/(val immutable: Int, var mutable: Int, nonpublic: Int) extends Product1[Int] {
      def _1() = {
        immutable
      }

      def canEqual(other: Any) = {
        other.isInstanceOf[introduceProductNTrait.nonPublicClassParams.Foo]
      }

      override def equals(other: Any) = {
        other match {
          case that: introduceProductNTrait.nonPublicClassParams.Foo => Foo.super.equals(that) && that.canEqual(Foo.this) && immutable == that.immutable
          case _ => false
        }
      }

      override def hashCode() = {
        val prime = 41
        prime * Foo.super.hashCode() + immutable.hashCode
      }
    }
    """
  } applyRefactoring(introduceProductNTrait(true, _ == "immutable", false))

}