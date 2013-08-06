package scala.tools.refactoring
package tests.implementations

import implementations.ChangeParamOrder
import tests.util.TestHelper
import tests.util.TestRefactoring

import language.reflectiveCalls

class ChangeParamOrderTest extends TestHelper with TestRefactoring {

  def changeParamOrder(permutations: List[List[Int]])(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ChangeParamOrder with SilentTracing with TestProjectIndex
    val changes = performRefactoring(permutations)
  }.changes

  @Test
  def exchangeTwoParams = new FileSet {
    """
      package changeParamOrder.exchangeTwoParamsTest
      class Foo {
        def /*(*/twoParams/*)*/(first: Int, second: Int) = second + first
      }
    """ becomes
    """
      package changeParamOrder.exchangeTwoParamsTest
      class Foo {
        def /*(*/twoParams/*)*/(second: Int, first: Int) = second + first
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil)))

  @Test
  def multipleParameterLists = new FileSet {
    """
      package changeParamOrder.multipleParameterLists
      class Foo {
        def /*(*/multipleParamLists/*)*/[A](first: Int, second: Int, third: Int)(flag: Boolean, f: Int => String, foo: A) = flag match {
          case true => f(first) + f(second) + third
          case false => third
        }
      }
    """ becomes
    """
      package changeParamOrder.multipleParameterLists
      class Foo {
        def /*(*/multipleParamLists/*)*/[A](second: Int, first: Int, third: Int)(f: Int => String, flag: Boolean, foo: A) = flag match {
          case true => f(first) + f(second) + third
          case false => third
        }
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil, 1::0::2::Nil)))

  @Test
  def methodCall = new FileSet {
    """
      package changeParamOrder.methodCall
      class Defining {
        def /*(*/method/*)*/(first: Int, second: String, third: Int) = second + first
      }
      class Calling {
        val defining = new Defining
        val result = defining.method(5, "asdf", 3)
      }
    """ becomes
    """
      package changeParamOrder.methodCall
      class Defining {
        def /*(*/method/*)*/(second: String, first: Int, third: Int) = second + first
      }
      class Calling {
        val defining = new Defining
        val result = defining.method("asdf", 5, 3)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil)))

  @Test
  def methodCallMultipleParamLists = new FileSet {
    """
      package changeParamOrder.methodCallMultipleParamLists
      class Defining {
        def /*(*/method/*)*/(first: Int, second: String, third: Int)(a: Int, b: Int, c: Int, d: Int) = second + first
      }
      class Calling {
        val defining = new Defining
        val result = defining.method(5, "asdf", 3)(1, 2, 3, 4)
      }
    """ becomes
    """
      package changeParamOrder.methodCallMultipleParamLists
      class Defining {
        def /*(*/method/*)*/(second: String, first: Int, third: Int)(b: Int, c: Int, a: Int, d: Int) = second + first
      }
      class Calling {
        val defining = new Defining
        val result = defining.method("asdf", 5, 3)(2, 3, 1, 4)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil, 1::2::0::3::Nil)))

  @Test
  def partialMethod = new FileSet {
    """
      package changeParamOrder.partialMethod
      class A {
        def /*(*/add/*)*/(a: Int, b: Int) = a + b
        def partial = add _
        val three = partial(1, 2)
      }
    """ becomes
    """
      package changeParamOrder.partialMethod
      class A {
        def /*(*/add/*)*/(b: Int, a: Int) = a + b
        def partial = add _
        val three = partial(2, 1)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil)))

  @Test
  def curriedPartialMethod = new FileSet {
    """
      package changeParamOrder.curriedPartialMethod
      class A {
        def /*(*/add5/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b + c + d + e
        def partial = add5(1, 2) _
        val ten = partial(3, 4, 5)
      }
    """ becomes
    """
      package changeParamOrder.curriedPartialMethod
      class A {
        def /*(*/add5/*)*/(b: Int, a: Int)(c: Int, e: Int, d: Int) = a + b + c + d + e
        def partial = add5(2, 1) _
        val ten = partial(3, 5, 4)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 0::2::1::Nil)))

  @Test
  def partialsWithBody = new FileSet {
    """
    package changeParamOrder.partialsWithBody
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
    """ becomes
    """
    package changeParamOrder.partialsWithBody
    class A {
      def /*(*/add/*)*/(b: Int, a: Int)(d: Int, e: Int, c: Int)(g: Int, h: Int, f: Int)(j: Int, i: Int) = a + b + c + d + e
      def firstPartial = {
        val a = 1
        val b = 2
        add(b, a) _
      }
      val firstResult = firstPartial(2, 3, 1)(2, 3, 1)(2, 1)
      def secondPartial = {
        val c = 1
        val d = 2
        val e = 3
        firstPartial(d, e, c)
      }
      val secondResult = secondPartial(2, 3, 1)(2, 1)
    }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 1::2::0::Nil, 1::2::0::Nil, 1::0::Nil)))

  @Test
  def partialValsWithBody = new FileSet {
    """
    package changeParamOrder.partialValsWithBody
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
    """ becomes
    """
    package changeParamOrder.partialValsWithBody
    class A {
      def /*(*/add/*)*/(b: Int, a: Int)(d: Int, e: Int, c: Int)(g: Int, h: Int, f: Int)(j: Int, i: Int) = a + b + c + d + e
      val firstPartial = {
        val a = 1
        val b = 2
        add(b, a) _
      }
      val firstResult = firstPartial(2, 3, 1)(2, 3, 1)(2, 1)
      val secondPartial = {
        val c = 1
        val d = 2
        val e = 3
        firstPartial(d, e, c)
      }
      val secondResult = secondPartial(2, 3, 1)(2, 1)
    }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 1::2::0::Nil, 1::2::0::Nil, 1::0::Nil)))

  @Test
  def curriedMethodCall = new FileSet {
    """
      package changeParamOrder.curriedMethodCall
      class Defining {
        def /*(*/method/*)*/(first: Int, second: String, third: Int)(a: Int, b: String, c: Int, d: Int) = second + first
      }
      class Currying {
        val defining = new Defining
        def curried = defining.method(5, "asdf", 3) _
        def curriedWithArgs(a: Int, b: String, c: Int, d: Int) = defining.method(1, "a", 2)(a, b, c, d)
        def fullyApplied = curried(1, "asdf", 2, 3)
      }
    """ becomes
    """
      package changeParamOrder.curriedMethodCall
      class Defining {
        def /*(*/method/*)*/(second: String, first: Int, third: Int)(b: String, c: Int, a: Int, d: Int) = second + first
      }
      class Currying {
        val defining = new Defining
        def curried = defining.method("asdf", 5, 3) _
        def curriedWithArgs(a: Int, b: String, c: Int, d: Int) = defining.method("a", 1, 2)(b, c, a, d)
        def fullyApplied = curried("asdf", 2, 1, 3)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil, 1::2::0::3::Nil)))

  @Test
  def repeatedlyPartiallyApplied = new FileSet {
    """
      package changeParamOrder.repeatedlyPartiallyApplied
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int) = a + b + c + d + e
        def firstPartial = add(1, 2) _
        def secondPartial = firstPartial(3, 4, 5)
        val result = secondPartial(6, 7, 8)
      }
    """ becomes
    """
      package changeParamOrder.repeatedlyPartiallyApplied
      class A {
        def /*(*/add/*)*/(b: Int, a: Int)(c: Int, e: Int, d: Int)(h: Int, g: Int, f: Int) = a + b + c + d + e
        def firstPartial = add(2, 1) _
        def secondPartial = firstPartial(3, 5, 4)
        val result = secondPartial(8, 7, 6)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 0::2::1::Nil, 2::1::0::Nil)))

  @Test
  def aliasToVal = new FileSet {
    """
      package changeParamOrder.aliasToVal
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b + c + d + e
        val alias = add _
        val result = alias(1, 2)(3, 4, 5)
      }
    """ becomes
    """
      package changeParamOrder.aliasToVal
      class A {
        def /*(*/add/*)*/(b: Int, a: Int)(c: Int, e: Int, d: Int) = a + b + c + d + e
        val alias = add _
        val result = alias(2, 1)(3, 5, 4)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 0::2::1::Nil)))

  @Test
  def partiallyAppliedVal = new FileSet {
    """
      package changeParamOrder.partiallyAppliedVal
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int) = a + b + c + d + e
        val partial = add(1, 2) _
        val result = partial(3, 4, 5)(6, 7, 8)
      }
    """ becomes
    """
      package changeParamOrder.partiallyAppliedVal
      class A {
        def /*(*/add/*)*/(b: Int, a: Int)(c: Int, e: Int, d: Int)(h: Int, g: Int, f: Int) = a + b + c + d + e
        val partial = add(2, 1) _
        val result = partial(3, 5, 4)(8, 7, 6)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 0::2::1::Nil, 2::1::0::Nil)))

  @Test
  def repeatedlyPartiallyAppliedVal = new FileSet {
    """
      package changeParamOrder.repeatedlyPartiallyAppliedVal
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int) = a + b + c + d + e
        val firstPartial = add(1, 2) _
        val secondPartial = firstPartial(3, 4, 5)
        val result = secondPartial(6, 7, 8)
      }
    """ becomes
    """
      package changeParamOrder.repeatedlyPartiallyAppliedVal
      class A {
        def /*(*/add/*)*/(b: Int, a: Int)(c: Int, e: Int, d: Int)(h: Int, g: Int, f: Int) = a + b + c + d + e
        val firstPartial = add(2, 1) _
        val secondPartial = firstPartial(3, 5, 4)
        val result = secondPartial(8, 7, 6)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 0::2::1::Nil, 2::1::0::Nil)))

  @Test
  def changeParamOrderSubclass = new FileSet {
    """
      package changeParamOrder.subclass
      class Parent {
        def /*(*/method/*)*/(a: Int, b: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        override def method(a: Int, b: Int, c: Int) = a*b*c
      }
    """ becomes
    """
      package changeParamOrder.subclass
      class Parent {
        def /*(*/method/*)*/(b: Int, a: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        override def method(b: Int, a: Int, c: Int) = a*b*c
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil)))

  @Test
  def changeParamOrderSuperclass = new FileSet {
    """
      package changeParamOrder.superclass
      class Parent {
        def method(a: Int, b: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        override def /*(*/method/*)*/(a: Int, b: Int, c: Int) = a*b*c
      }
    """ becomes
    """
      package changeParamOrder.superclass
      class Parent {
        def method(b: Int, a: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        override def /*(*/method/*)*/(b: Int, a: Int, c: Int) = a*b*c
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil)))

  @Test
  def changeParamOrderSuperClassCall = new FileSet {
    """
      package changeParamOrder.superClassCall
      class Parent {
        def /*(*/method/*)*/(a: Int, b: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        val sum3 = method(1, 2, 3)
      }
    """ becomes
    """
      package changeParamOrder.superClassCall
      class Parent {
        def /*(*/method/*)*/(b: Int, a: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        val sum3 = method(2, 1, 3)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil)))

  @Test(expected=classOf[RefactoringException])
  def invalidPermutation = new FileSet {
    """
    package changeParamOrder.invalidPermutation
    class Foo {
      def /*(*/method/*)*/(a: Int, b: Int)(c: String, d: String, e: String)(f: Int, g: Int) = a+b+c+d+e+f+g
    }
    """ becomes
    """
    not relevant
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 1::0::3::Nil, 0::1::Nil)))

  @Test
  def partiallyAppliedMethodUsage = new FileSet {
    """
      package splitParameterLists.partiallyAppliedMethodUsage
      class A {
        def /*(*/curriedAdd5/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b + c + d
        def alias(a: Int, b: Int) = curriedAdd5(a, b) _
        val result = alias(1, 2)(3, 4, 5)
      }
    """ becomes
    """
      package splitParameterLists.partiallyAppliedMethodUsage
      class A {
        def /*(*/curriedAdd5/*)*/(b: Int, a: Int)(c: Int, e: Int, d: Int) = a + b + c + d
        def alias(a: Int, b: Int) = curriedAdd5(b, a) _
        val result = alias(1, 2)(3, 5, 4)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 0::2::1::Nil)))

  @Test
  def partiallyAppliedMethodUsage2 = new FileSet {
    """
      package splitParameterLists.partiallyAppliedMethodUsage2
      class A {
        def /*(*/curriedAdd9/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int, j: Int) = a + b + c + d + e + f
        def alias = curriedAdd9 _
        def partial(a: Int, b: Int) = alias(a, b)
        val result = partial(1, 2)(3, 4, 5)(6, 7, 8, 9)
      }
    """ becomes
    """
      package splitParameterLists.partiallyAppliedMethodUsage2
      class A {
        def /*(*/curriedAdd9/*)*/(b: Int, a: Int)(e: Int, c: Int, d: Int)(j: Int, f: Int, g: Int, h: Int) = a + b + c + d + e + f
        def alias = curriedAdd9 _
        def partial(a: Int, b: Int) = alias(b, a)
        val result = partial(1, 2)(5, 3, 4)(9, 6, 7, 8)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil, 2::0::1::Nil, 3::0::1::2::Nil)))

}