package scala.tools.refactoring
package tests.implementations


import implementations.SplitParameterLists
import tests.util.TestHelper
import tests.util.TestRefactoring
import org.junit.Ignore

class SplitParameterListsTest extends TestHelper with TestRefactoring {

  outer => 

  import outer.global._
    
  def splitParameterLists(splitPositions: List[List[Int]])(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new SplitParameterLists with SilentTracing with GlobalIndexes {
      val global = outer.global
      val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
      val index = GlobalIndex(cuIndexes)
    }
    val changes = performRefactoring(splitPositions)
  }.changes
  
  @Test
  def simpleCurrying = new FileSet {
    """
      package splitParameterLists.simpleCurrying
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int) = first + second
      }
    """ becomes
    """
      package splitParameterLists.simpleCurrying
	  class A {
        def /*(*/add/*)*/(first: Int)(second: Int) = first + second
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil)))
  
  @Test
  def multipleParamListCurrying = new FileSet {
    """
      package splitParameterLists.multipleParamListCurrying
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = first + second
      }
    """ becomes
    """
      package splitParameterLists.multipleParamListCurrying
	  class A {
        def /*(*/add/*)*/(first: Int)(second: Int)(a: String)(b: String)(c: String) = first + second
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 1::2::Nil)))
  
  @Test
  def curryingWithMethodCall = new FileSet {
    """
      package splitParameterLists.curryingWithMethodCall
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = first + second
      }
	  class B {
        val a = new A
        val b = a.add(1, 2)("a", "b", "c")
      }
    """ becomes
    """
      package splitParameterLists.curryingWithMethodCall
	  class A {
        def /*(*/add/*)*/(first: Int)(second: Int)(a: String, b: String)(c: String) = first + second
      }
	  class B {
        val a = new A
        val b = a.add(1)(2)("a", "b")("c")
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil)))
  
  @Test
  def curryingMethodSubclass = new FileSet {
    """
      package splitParameterLists.curryingMethodSubclass
      class Parent {
        def /*(*/method/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = (first + second, a+b+c)
      }

      class Child extends Parent {
        override def method(first: Int, second: Int)(a: String, b: String, c: String) = (first, a)
      }
    """ becomes
    """
      package splitParameterLists.curryingMethodSubclass
      class Parent {
        def /*(*/method/*)*/(first: Int)(second: Int)(a: String, b: String)(c: String) = (first + second, a+b+c)
      }

      class Child extends Parent {
        override def method(first: Int)(second: Int)(a: String, b: String)(c: String) = (first, a)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil)))
  
  @Test
  def curryingMethodSuperclass = new FileSet {
    """
      package splitParameterLists.curryingMethodSuperclass
      class Parent {
        def method(first: Int, second: Int)(a: String, b: String, c: String) = (first + second, a+b+c)
      }

      class Child extends Parent {
        override def /*(*/method/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = (first, a)
      }
    """ becomes
    """
      package splitParameterLists.curryingMethodSuperclass
      class Parent {
        def method(first: Int)(second: Int)(a: String, b: String)(c: String) = (first + second, a+b+c)
      }

      class Child extends Parent {
        override def /*(*/method/*)*/(first: Int)(second: Int)(a: String, b: String)(c: String) = (first, a)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil)))
  
  @Test
  def curriedMethodAliased = new FileSet {
    """
      package splitParameterLists.curriedMethodAliased
      class A {
        def /*(*/curriedAdd3/*)*/(a: Int, b: Int, c: Int) = a + b + c
        def alias = curriedAdd3 _
        val six = alias(1, 2, 3)
      }
    """ becomes
    """
      package splitParameterLists.curriedMethodAliased
      class A {
        def /*(*/curriedAdd3/*)*/(a: Int)(b: Int)(c: Int) = a + b + c
        def alias = curriedAdd3 _
        val six = alias(1)(2)(3)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::2::Nil)))  
  
  @Test
  def curriedMethodAliasedTwoParamLists = new FileSet {
    """
      package splitParameterLists.curriedMethodAliasedTwoParamLists
      class A {
        def /*(*/curriedAdd4/*)*/(a: Int, b: Int)(c: Int, d: Int) = a + b + c + d
        def alias = curriedAdd4 _
        val ten = alias(1, 2)(3, 4)
      }
    """ becomes
    """
      package splitParameterLists.curriedMethodAliasedTwoParamLists
      class A {
        def /*(*/curriedAdd4/*)*/(a: Int)(b: Int)(c: Int)(d: Int) = a + b + c + d
        def alias = curriedAdd4 _
        val ten = alias(1)(2)(3)(4)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 1::Nil)))
  
  @Test
  def curriedMethodPartiallyApplied = new FileSet {
    """
      package splitParameterLists.curriedMethodPartiallyApplied
      class A {
        def /*(*/curriedAdd5/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b + c + d + e
        def partial = curriedAdd5(1, 2) _
        val fifteen = partial(3, 4, 5)
      }
    """ becomes
    """
      package splitParameterLists.curriedMethodPartiallyApplied
      class A {
        def /*(*/curriedAdd5/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int) = a + b + c + d + e
        def partial = curriedAdd5(1)(2) _
        val fifteen = partial(3, 4)(5)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil)))
  
  @Test
  def partiallyCurried = new FileSet {
    """
      package splitParameterLists.partiallyCurried
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int) = a + b + c + d + e
        def partial = add(1, 2) _
        val result = partial(3, 4, 5)(6, 7, 8)
      }
    """ becomes
    """
      package splitParameterLists.partiallyCurried
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int, h: Int) = a + b + c + d + e
        def partial = add(1)(2) _
        val result = partial(3, 4)(5)(6)(7, 8)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil, 1::Nil)))
  
  @Test
  def twoPartiallyCurriedMethods = new FileSet {
    """
      package splitParameterLists.twoPartiallyCurriedMethods
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int) = a + b + c + d + e
        def first = add(1, 2) _
        def second = add(1, 2)(3, 4, 5) _
        val result1 = first(3, 4, 5)(6, 7, 8)
        val result2 = second(6, 7, 8)
      }
    """ becomes
    """
      package splitParameterLists.twoPartiallyCurriedMethods
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int, h: Int) = a + b + c + d + e
        def first = addaddaddaddaddaddadd(1)(2) _
        def second = add(1)(2)(3, 4)(5) _
        val result1 = first(3, 4)(5)(6)(7, 8)
        val result2 = second(6)(7, 8)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil, 1::Nil)))
  
  @Test
  def repeatedlyPartiallyApplied = new FileSet {
    """
      package splitParameterLists.repeatedlyPartiallyApplied
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int, h: Int)(i: Int, j: Int) = a + b + c + d + e
        def firstPartial = add(1, 2) _
        def secondPartial = firstPartial(3, 4, 5)
        def thirdPartial = secondPartial(6, 7, 8)
        val result = thirdPartial(9, 10)
      }
    """ becomes
    """
      package splitParameterLists.repeatedlyPartiallyApplied
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int, h: Int) = a + b + c + d + e
        def firstPartial = add(1)(2) _
        def secondPartial = firstPartial(3, 4)(5)
        def thirdPartial = secondPartial(6)(7, 8)
        val result = thirdPartial(9)(10)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil, 1::Nil, 1::Nil)))
  
  @Test
  def aliasToVal = new FileSet {
    """
      package splitParameterLists.aliasToVal
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int) = a + b + c + d + e
        val alias = add _
        val result = alias(1, 2)(3, 4, 5)
      }
    """ becomes
    """
      package splitParameterLists.aliasToVal
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int) = a + b + c + d + e
        val alias = add _
        val result = alias(1)(2)(3, 4)(5)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil)))
  
  @Test
  def repeatedlyPartiallyAppliedVal = new FileSet {
    """
      package splitParameterLists.repeatedlyPartiallyAppliedVal
      class A {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int, e: Int)(f: Int, g: Int) = a + b + c + d + e + f + g
        val firstPartial = add(1, 2) _
        val secondPartial = firstPartial(3, 4, 5)
        val result = secondPartial(6, 7)
      }
    """ becomes
    """
      package splitParameterLists.repeatedlyPartiallyAppliedVal
      class A {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int, d: Int)(e: Int)(f: Int)(g: Int) = a + b + c + d + e + f + g
        val firstPartial = add(1)(2) _
        val secondPartial = firstPartial(3, 4)(5)
        val result = secondPartial(6)(7)
      }
    """ 
  } applyRefactoring(splitParameterLists(List(1::Nil, 2::Nil, 1::Nil)))
  
  @Test
  @Ignore // TODO: implement
  def partialOverride = new FileSet {
    """
      package splitParameterLists.partialOverride
      class Parent {
        def /*(*/add/*)*/(a: Int, b: Int)(c: Int, d: Int) = a + b + c + d
        def partial = add(1, 2) _
      }
      class Child {
        override def partial = (a, b) => a*b
      }
    """ becomes
    """
      package splitParameterLists.partialOverride
      class Parent {
        def /*(*/add/*)*/(a: Int)(b: Int)(c: Int)(d: Int) = a + b + c + d
        def partial = add(1)(2) _
      }
      class Child {
        override def partial = a => b => a*b
      }
    """
  } applyRefactoring(splitParameterLists(List(1::Nil, 1::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def unorderedSplitPositions = new FileSet {
    """
      package splitParameterLists.unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package splitParameterLists.unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(splitParameterLists(List(2::1::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def aboveBoundsSplitPosition = new FileSet {
    """
      package splitParameterLists.aboveBoundsSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package splitParameterLists.aboveBoundsSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(splitParameterLists(List(3::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def belowBoundsSplitPosition = new FileSet {
    """
      package splitParameterLists.belowBoundsSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package splitParameterLists.belowBoundsSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(splitParameterLists(List(0::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def duplicatedSplitPosition = new FileSet {
    """
      package splitParameterLists.duplicatedSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package splitParameterLists.duplicatedSplitPosition
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(splitParameterLists(List(1::1::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def tooManySplitPositions = new FileSet {
    """
      package splitParameterLists.tooManySplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package splitParameterLists.tooManySplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(splitParameterLists(List(1::Nil, 1::Nil)))
  
}