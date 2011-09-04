package scala.tools.refactoring
package tests.implementations


import implementations.CurryMethod
import tests.util.TestHelper
import tests.util.TestRefactoring
import org.junit.Ignore

class CurryMethodTest extends TestHelper with TestRefactoring {

  outer => 

  import outer.global._
    
  def curryMethod(splitPositions: List[List[Int]])(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new CurryMethod with SilentTracing with GlobalIndexes {
      val global = outer.global
      val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
      val index = GlobalIndex(cuIndexes)
    }
    val changes = performRefactoring(splitPositions)
  }.changes
  
  @Test
  def simpleCurrying = new FileSet {
    """
      package curryMethod.simpleCurrying
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int) = first + second
      }
    """ becomes
    """
      package curryMethod.simpleCurrying
	  class A {
        def /*(*/add/*)*/(first: Int)(second: Int) = first + second
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil)))
  
  @Test
  def multipleParamListCurrying = new FileSet {
    """
      package curryMethod.multipleParamListCurrying
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = first + second
      }
    """ becomes
    """
      package curryMethod.multipleParamListCurrying
	  class A {
        def /*(*/add/*)*/(first: Int)(second: Int)(a: String)(b: String)(c: String) = first + second
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 1::2::Nil)))
  
  @Test
  def curryingWithMethodCall = new FileSet {
    """
      package curryMethod.curryingWithMethodCall
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = first + second
      }
	  class B {
        val a = new A
        val b = a.add(1, 2)("a", "b", "c")
      }
    """ becomes
    """
      package curryMethod.curryingWithMethodCall
	  class A {
        def /*(*/add/*)*/(first: Int)(second: Int)(a: String, b: String)(c: String) = first + second
      }
	  class B {
        val a = new A
        val b = a.add(1)(2)("a", "b")("c")
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 2::Nil)))
  
  @Test
  def curryingMethodSubclass = new FileSet {
    """
      package curryMethod.curryingMethodSubclass
      class Parent {
        def /*(*/method/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = (first + second, a+b+c)
      }

      class Child extends Parent {
        override def method(first: Int, second: Int)(a: String, b: String, c: String) = (first, a)
      }
    """ becomes
    """
      package curryMethod.curryingMethodSubclass
      class Parent {
        def /*(*/method/*)*/(first: Int)(second: Int)(a: String, b: String)(c: String) = (first + second, a+b+c)
      }

      class Child extends Parent {
        override def method(first: Int)(second: Int)(a: String, b: String)(c: String) = (first, a)
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 2::Nil)))
  
  @Test
  def curryingMethodSuperclass = new FileSet {
    """
      package curryMethod.curryingMethodSuperclass
      class Parent {
        def method(first: Int, second: Int)(a: String, b: String, c: String) = (first + second, a+b+c)
      }

      class Child extends Parent {
        override def /*(*/method/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = (first, a)
      }
    """ becomes
    """
      package curryMethod.curryingMethodSuperclass
      class Parent {
        def method(first: Int)(second: Int)(a: String, b: String)(c: String) = (first + second, a+b+c)
      }

      class Child extends Parent {
        override def /*(*/method/*)*/(first: Int)(second: Int)(a: String, b: String)(c: String) = (first, a)
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 2::Nil)))
  
  @Test
  def curriedMethodAliased = new FileSet {
    """
      package curryMethod.curriedMethodAliased
      class A {
        def /*(*/curriedAdd3/*)*/(a: Int, b: Int, c: Int) = a + b + c
        def alias = curriedAdd3 _
        val six = alias(1, 2, 3)
      }
    """ becomes
    """
      package curryMethod.curriedMethodAliased
      class A {
        def /*(*/curriedAdd3/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
        def alias = curriedAdd3 _
        val six = alias(1)(2)(3)
      }
    """ 
  } applyRefactoring(curryMethod(List(1::2::Nil)))  
  
  @Test
  def curriedMethodAliasedTwoParamLists = new FileSet {
    """
      package curryMethod.curriedMethodAliasedTwoParamLists
      class A {
        def /*(*/curriedAdd4/*)*/(a: Int, b: Int)(c: Int, d: Int) = a + b + c + d
        def alias = curriedAdd4 _
        val ten = alias(1, 2)(3, 4)
      }
    """ becomes
    """
      package curryMethod.curriedMethodAliasedTwoParamLists
      class A {
        def /*(*/curriedAdd4/*)*/(a: Int)(b: Int)(c: Int)(d: Int) = a + b + c + d
        def alias = curriedAdd4 _
        val ten = alias(1)(2)(3)(4)
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 1::Nil)))
  
  @Test
  def curriedMethodPartiallyApplied = new FileSet {
    """
      package curryMethod.curriedMethodPartiallyApplied
      class A {
        def /*(*/curriedAdd5/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b + c + d + e
        def partial = curriedAdd5(1, 2) _
        val fifteen = partial(3, 4, 5)
      }
    """ becomes
    """
      package curryMethod.curriedMethodPartiallyApplied
      class A {
        def /*(*/curriedAdd5/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int) = a + b + c + d + e
        def partial = curriedAdd5(1)(2) _
        val fifteen = partial(3, 4)(5)
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 2::Nil)))
  
  @Test
  def partiallyCurried = new FileSet {
    """
      package curryMethod.partiallyCurried
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int) = a + b + c + d + e
        def partial = /*FIXME*/add(1, 2) _
        val result = partial(3, 4, 5)(6, 7, 8)
      }
    """ becomes
    """
      package curryMethod.partiallyCurried
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int, h: Int) = a + b + c + d + e
        def partial = /*FIXME*/add(1)(2) _
        val result = partial(3, 4)(5)(6)(7, 8)
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 2::Nil, 1::Nil)))
  
  @Test
  def twoPartiallyCurriedMethods = new FileSet {
    """
      package curryMethod.twoPartiallyCurriedMethods
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int) = a + b + c + d + e
        def first = add(1, 2) _
        def second = add(1, 2)(3, 4, 5) _
        val result1 = first(3, 4, 5)(6, 7, 8)
        val result2 = second(6, 7, 8)
      }
    """ becomes
    """
      package curryMethod.twoPartiallyCurriedMethods
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int, h: Int) = a + b + c + d + e
        def first = addaddaddaddaddaddadd(1)(2) _
        def second = add(1)(2)(3, 4)(5) _
        val result1 = first(3, 4)(5)(6)(7, 8)
        val result2 = second(6)(7, 8)
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 2::Nil, 1::Nil)))
  
  @Test
  def repeatedlyPartiallyApplied = new FileSet {
    """
      package curryMethod.repeatedlyPartiallyApplied
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int)(i: Int, j: Int) = a + b + c + d + e
        def firstPartial = add(1, 2) _
        def secondPartial = firstPartial(3, 4, 5)
        def thirdPartial = secondPartial(6, 7, 8)
        val result = thirdPartial(9, 10)
      }
    """ becomes
    """
      package curryMethod.repeatedlyPartiallyApplied
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int, h: Int) = a + b + c + d + e
        def firstPartial = add(1)(2) _
        def secondPartial = firstPartial(3, 4)(5)
        def thirdPartial = secondPartial(6)(7, 8)
        val result = thirdPartial(9)(10)
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 2::Nil, 1::Nil, 1::Nil)))
  
  @Test
  def dummy = {
    val tree = treeFrom {
      """
      package curryMethod.partiallyCurried
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int)(g: Int, h: Int) = a + b + c + d + e
        def partial = add(1, 2) _
        def partial2 = partial(3, 4, 5)
        val result = partial2(6)(7, 8)
      }
      """
    }
    println("foo")
  }
  
  @Test(expected=classOf[RefactoringException])
  def unorderedSplitPositions = new FileSet {
    """
      package curryMethod.unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package curryMethod.unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(2::1::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def aboveBoundsSplitPosition = new FileSet {
    """
      package curryMethod.aboveBoundsSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package curryMethod.aboveBoundsSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(3::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def belowBoundsSplitPosition = new FileSet {
    """
      package curryMethod.belowBoundsSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package curryMethod.belowBoundsSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(0::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def duplicatedSplitPosition = new FileSet {
    """
      package curryMethod.duplicatedSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package curryMethod.duplicatedSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(1::1::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def tooManySplitPositions = new FileSet {
    """
      package curryMethod.tooManySplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package curryMethod.tooManySplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(1::Nil, 1::Nil)))
  
}