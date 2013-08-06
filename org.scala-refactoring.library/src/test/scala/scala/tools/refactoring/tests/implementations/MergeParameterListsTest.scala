package scala.tools.refactoring
package tests.implementations

import implementations.MergeParameterLists
import tests.util.TestHelper
import tests.util.TestRefactoring
import org.junit.Assert

import language.reflectiveCalls

class MergeParameterListsTest extends TestHelper with TestRefactoring {

  outer =>

  import outer.global._

  def mergeParameterLists(mergePositions: List[Int])(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new MergeParameterLists with SilentTracing with TestProjectIndex
    val changes = performRefactoring(mergePositions)
  }.changes


  @Test(expected=classOf[RefactoringException])
  def tooSmallMergePosition = new FileSet {
    """
    package mergeParameterLists.tooSmallMergePosition
    class A {
      def /*(*/foo/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
    }
    """ becomes
    """
    package mergeParameterLists.tooSmallMergePosition
    class A {
      def /*(*/foo/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
    }
    """
  } applyRefactoring(mergeParameterLists(0::1::Nil))

  @Test(expected=classOf[RefactoringException])
  def tooBigMergePosition = new FileSet {
    """
    package mergeParameterLists.tooBigMergePosition
    class A {
      def /*(*/foo/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
    }
    """ becomes
    """
    package mergeParameterLists.tooBigMergePosition
    class A {
      def /*(*/foo/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
    }
    """
  } applyRefactoring(mergeParameterLists(1::3::Nil))

  @Test(expected=classOf[RefactoringException])
  def unsortedMergePositions = new FileSet {
    """
    package mergeParameterLists.unsortedMergePositions
    class A {
      def /*(*/foo/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
    }
    """ becomes
    """
    package mergeParameterLists.unsortedMergePositions
    class A {
      def /*(*/foo/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
    }
    """
  } applyRefactoring(mergeParameterLists(2::1::Nil))

  @Test(expected=classOf[RefactoringException])
  def repeatedMergePosition = new FileSet {
    """
    package mergeParameterLists.repeatedMergePosition
    class A {
      def /*(*/foo/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
    }
    """ becomes
    """
    package mergeParameterLists.repeatedMergePosition
    class A {
      def /*(*/foo/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
    }
    """
  } applyRefactoring(mergeParameterLists(1::1::2::Nil))


  @Test
  def mergeAllLists = new FileSet {
    """
    package mergeParameterLists.mergeAllLists
    class A {
      def /*(*/toMerge/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int) = a + b
    }
    """ becomes
    """
    package mergeParameterLists.mergeAllLists
    class A {
      def /*(*/toMerge/*)*/(a: Int, b: Int, c: Int, d: Int, e: Int) = a + b
    }
    """
  } applyRefactoring(mergeParameterLists(1::2::3::Nil))

  @Test
  def mergeSomeLists = new FileSet {
    """
    package mergeParameterLists.mergeSomeLists
    class A {
      def /*(*/toMerge/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int) = a + b
    }
    """ becomes
    """
    package mergeParameterLists.mergeSomeLists
    class A {
      def /*(*/toMerge/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b
    }
    """
  } applyRefactoring(mergeParameterLists(1::3::Nil))

  @Test
  def mergeWithCall = new FileSet {
    """
    package mergeParameterLists.mergeWithCall
    class A {
      def /*(*/toMerge/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int) = a + b
      val x = toMerge(1)(2)(3, 4)(5)
    }
    """ becomes
    """
    package mergeParameterLists.mergeWithCall
    class A {
      def /*(*/toMerge/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b
      val x = toMerge(1, 2)(3, 4, 5)
    }
    """
  } applyRefactoring(mergeParameterLists(1::3::Nil))

  @Test
  def mergeMethodSubclass = new FileSet {
    """
    package mergeParameterLists.mergeMethodSubclass
    class Parent {
      def /*(*/method/*)*/(first: Int)(second: Int)(a: Int, b: Int)(c: Int) = first + second
    }
    class Child extends Parent {
      override def method(first:Int)(second: Int)(a: Int, b: Int)(c: Int) = a + b + c
    }
    """ becomes
    """
    package mergeParameterLists.mergeMethodSubclass
    class Parent {
      def /*(*/method/*)*/(first: Int, second: Int)(a: Int, b: Int, c: Int) = first + second
    }
    class Child extends Parent {
      override def method(first:Int, second: Int)(a: Int, b: Int, c: Int) = a + b + c
    }
    """
  } applyRefactoring(mergeParameterLists(1::3::Nil))

  @Test
  def mergeMethodSuperclass = new FileSet {
    """
    package mergeParameterLists.mergeMethodSuperclass
    class Parent {
      def method(first: Int)(second: Int)(a: Int, b: Int)(c: Int) = first + second
    }
    class Child extends Parent {
      override def /*(*/method/*)*/(first:Int)(second: Int)(a: Int, b: Int)(c: Int) = a + b + c
    }
    """ becomes
    """
    package mergeParameterLists.mergeMethodSuperclass
    class Parent {
      def method(first: Int, second: Int)(a: Int, b: Int, c: Int) = first + second
    }
    class Child extends Parent {
      override def /*(*/method/*)*/(first:Int, second: Int)(a: Int, b: Int, c: Int) = a + b + c
    }
    """
  } applyRefactoring(mergeParameterLists(1::3::Nil))

  @Test
  def mergeMethodAliased = new FileSet {
    """
    package mergeParameterLists.mergeMethodAliased
    class A {
      def /*(*/method/*)*/(a: Int)(b: Int)(c: Int)(d: Int) = a + b + c + d
      def alias = method _
      val ten = method(1)(2)(3)(4)
    }
    """ becomes
    """
    package mergeParameterLists.mergeMethodAliased
    class A {
      def /*(*/method/*)*/(a: Int, b: Int, c: Int)(d: Int) = a + b + c + d
      def alias = method _
      val ten = method(1, 2, 3)(4)
    }
    """
  } applyRefactoring(mergeParameterLists(1::2::Nil))

  @Test
  def mergePartiallyApplied= new FileSet {
    """
    package mergeParameterLists.mergePartiallyApplied
    class A {
      def /*(*/method/*)*/(a: Int)(b: Int)(c: Int)(d: Int) = a + b + c + d
      def partial = method(1)(2) _
      val ten = partial(3)(4)
    }
    """ becomes
    """
    package mergeParameterLists.mergePartiallyApplied
    class A {
      def /*(*/method/*)*/(a: Int, b: Int)(c: Int, d: Int) = a + b + c + d
      def partial = method(1, 2) _
      val ten = partial(3, 4)
    }
    """
  } applyRefactoring(mergeParameterLists(1::3::Nil))

  @Test
  def repeatedlyPartiallyApplied = new FileSet {
    """
    package mergeParameterLists.repeatedlyPartiallyApplied
    class A {
      def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int, h: Int)(i: Int)(j: Int) = a + b + c + d + e
      def firstPartial = add(1)(2) _
      def secondPartial = firstPartial(3, 4)(5)
      def thirdPartial = secondPartial(6)(7, 8)
      val result = thirdPartial(9)(10)
    }
    """  becomes
    """
    package mergeParameterLists.repeatedlyPartiallyApplied
    class A {
      def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int)(i: Int, j: Int) = a + b + c + d + e
      def firstPartial = add(1, 2) _
      def secondPartial = firstPartial(3, 4, 5)
      def thirdPartial = secondPartial(6, 7, 8)
      val result = thirdPartial(9, 10)
    }
    """
  } applyRefactoring(mergeParameterLists(1::3::5::7::Nil))

  @Test
  def aliasToVal = new FileSet {
    """
      package mergeParameterLists.aliasToVal
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int) = a + b + c + d + e
        val alias = add _
        val result = alias(1)(2)(3, 4)(5)
      }
    """  becomes
    """
      package mergeParameterLists.aliasToVal
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b + c + d + e
        val alias = add _
        val result = alias(1, 2)(3, 4, 5)
      }
    """
  } applyRefactoring(mergeParameterLists(1::3::Nil))

  @Test
  def repeatedlyPartiallyAppliedVal = new FileSet {
    """
      package mergeParameterLists.repeatedlyPartiallyAppliedVal
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int) = a + b + c + d + e + f + g
        val firstPartial = add(1)(2) _
        val secondPartial = firstPartial(3, 4)(5)
        val result = secondPartial(6)(7)
      }
    """ becomes
    """
      package mergeParameterLists.repeatedlyPartiallyAppliedVal
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int) = a + b + c + d + e + f + g
        val firstPartial = add(1, 2) _
        val secondPartial = firstPartial(3, 4, 5)
        val result = secondPartial(6, 7)
      }
    """
  } applyRefactoring(mergeParameterLists(1::3::5::Nil))

  @Test
  def partialsWithBody = new FileSet {
    """
    package mergeParameterLists.partialsWithBody
    class A {
      def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int)(h: Int)(i: Int)(j: Int) = a + b + c + d + e
      def firstPartial = {
        val a = 1
        val b = 2
        add(a)(b) _
      }
      val firstResult = firstPartial(1, 2)(3)(1)(2)(3)(1)(2)
      def secondPartial = {
        val c = 1
        val d = 2
        val e = 3
        firstPartial(c, d)(e)
      }
      val secondResult = secondPartial(1)(2)(3)(1)(2)
    }
    """ becomes
    """
    package mergeParameterLists.partialsWithBody
    class A {
      def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int)(i: Int, j: Int) = a + b + c + d + e
      def firstPartial = {
        val a = 1
        val b = 2
        add(a, b) _
      }
      val firstResult = firstPartial(1, 2, 3)(1, 2, 3)(1, 2)
      def secondPartial = {
        val c = 1
        val d = 2
        val e = 3
        firstPartial(c, d, e)
      }
      val secondResult = secondPartial(1, 2, 3)(1, 2)
    }
    """
  } applyRefactoring(mergeParameterLists(List(1, 3, 5, 6, 8)))

  @Test
  def partialValsWithBody = new FileSet {
    """
    package mergeParameterLists.partialValsWithBody
    class A {
      def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int)(h: Int)(i: Int)(j: Int) = a + b + c + d + e
      val firstPartial = {
        val a = 1
        val b = 2
        add(a)(b) _
      }
      val firstResult = firstPartial(1, 2)(3)(1)(2)(3)(1)(2)
      val secondPartial = {
        val c = 1
        val d = 2
        val e = 3
        firstPartial(c, d)(e)
      }
      val secondResult = secondPartial(1)(2)(3)(1)(2)
    }
    """ becomes
    """
    package mergeParameterLists.partialValsWithBody
    class A {
      def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int)(i: Int, j: Int) = a + b + c + d + e
      val firstPartial = {
        val a = 1
        val b = 2
        add(a, b) _
      }
      val firstResult = firstPartial(1, 2, 3)(1, 2, 3)(1, 2)
      val secondPartial = {
        val c = 1
        val d = 2
        val e = 3
        firstPartial(c, d, e)
      }
      val secondResult = secondPartial(1, 2, 3)(1, 2)
    }
    """
  } applyRefactoring(mergeParameterLists(List(1, 3, 5, 6, 8)))

  @Test(expected=classOf[RefactoringException])
  def mergePointUsedForCurrying = new FileSet {
    """
    package mergeParameterLists.mergePointUsedForCurrying
    class A {
      def /*(*/method/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
      val curried = method(1)(2) _
    }
    """ becomes
    """
    package mergeParameterLists.mergePointUsedForCurrying
    class A {
      def /*(*/method/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
      val curried = method(1)(2) _
    }
    """
  } applyRefactoring(mergeParameterLists(1::2::Nil))

  @Test
  def partiallyAppliedMethodUsage = new FileSet {
    """
      package splitParameterLists.partiallyAppliedMethodUsage
      class A {
        def /*(*/curriedAdd4/*)*/(a: Int)(b: Int)(c: Int)(d: Int) = a + b + c + d
        def alias(a: Int, b: Int) = curriedAdd4(a)(b) _
        val ten = alias(1, 2)(3)(4)
      }
    """ becomes
    """
      package splitParameterLists.partiallyAppliedMethodUsage
      class A {
        def /*(*/curriedAdd4/*)*/(a: Int, b: Int)(c: Int, d: Int) = a + b + c + d
        def alias(a: Int, b: Int) = curriedAdd4(a, b) _
        val ten = alias(1, 2)(3, 4)
      }
    """
  } applyRefactoring(mergeParameterLists(1::3::Nil))

  @Test
  def partiallyAppliedMethodUsage2 = new FileSet {
    """
      package splitParameterLists.partiallyAppliedMethodUsage2
      class A {
        def /*(*/curriedAdd6/*)*/(a: Int)(b: Int)(c: Int)(d: Int)(e: Int)(f: Int) = a + b + c + d + e + f
        def alias = curriedAdd6 _
        def partial(a: Int, b: Int) = alias(a)(b)
        val result = partial(1,2)(3)(4)(5)(6)
      }
    """ becomes
    """
      package splitParameterLists.partiallyAppliedMethodUsage2
      class A {
        def /*(*/curriedAdd6/*)*/(a: Int, b: Int)(c: Int, d: Int)(e: Int, f: Int) = a + b + c + d + e + f
        def alias = curriedAdd6 _
        def partial(a: Int, b: Int) = alias(a, b)
        val result = partial(1,2)(3, 4)(5, 6)
      }
    """
  } applyRefactoring(mergeParameterLists(1::3::5::Nil))

}