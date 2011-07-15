package scala.tools.refactoring
package tests.implementations

import implementations.CurryMethod
import tests.util.TestHelper
import tests.util.TestRefactoring

class CurryMethodTest extends TestHelper with TestRefactoring {

  outer => 
    
  def curryMethod(splitPositions: List[List[Int]])(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new CurryMethod with SilentTracing {
      val global = outer.global
    }
    val changes = performRefactoring(splitPositions)
  }.changes
  
  @Test
  def simpleCurrying = new FileSet {
    """
      package simpleCurrying
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int) = first + second
      }
    """ becomes
    """
      package simpleCurrying
	  class A {
        def /*(*/add/*)*/(first: Int)( second: Int) = first + second
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil)))
  
  @Test
  def multipleParamListCurrying = new FileSet {
    """
      package multipleParamListCurrying
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = first + second
      }
    """ becomes
    """
      package multipleParamListCurrying
	  class A {
        def /*(*/add/*)*/(first: Int)( second: Int)(a: String)( b: String)( c: String) = first + second
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 1::2::Nil)))
  
  @Test
  def curryingWithMethodCall = new FileSet {
    """
      package curryingWithMethodCall
	  class A {
        def /*(*/add/*)*/(first: Int, second: Int)(a: String, b: String, c: String) = first + second
      }
	  class B {
        val a = new A
        val b = a.add(1, 2)("a", "b", "c")
      }
    """ becomes
    """
      package curryingWithMethodCall
	  class A {
        def /*(*/add/*)*/(first: Int)( second: Int)(a: String, b: String)( c: String) = first + second
      }
	  class B {
        val a = new A
        val b = a.add(1)( 2)("a",  "b")( "c")
      }
    """ 
  } applyRefactoring(curryMethod(List(1::Nil, 2::Nil)))
  
  @Test // TODO
  def curriedMethodAliased= new FileSet {
    """
      package curriedMethodAliased
      class A {
        def /*(*/curriedAdd3/*)*/(a: Int)(b: Int, c: Int) = a + b + c
        def alias = curriedAdd3
        val six = alias(1)(2, 3)
      }
    """ becomes
    """
      package curriedMethodAliased
      class A {
        def /*(*/curriedAdd3/*)*/(a: Int)(b: Int)( c: Int) = a + b + c
        def alias = curriedAdd3
        val six = curriedAdd(1)(2)(3)
      }
    """ 
  } applyRefactoring(curryMethod(List(Nil, 1::Nil)))
  
  @Test
  def alias = {
    val tree = treeFrom {
          """
      package curriedMethodAliased
      class A {
        def /*(*/curriedAdd3/*)*/(a: Int)(b: Int)( c: Int) = a + b + c
        def alias = curriedAdd3 _
        val six = alias(1)(2)(3)
      }
    """
    }
    println(57)
  }
  
  
  @Test(expected=classOf[RefactoringException])
  def unorderedSplitPositions = new FileSet {
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(2::1::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def aboveBoundsSplitPosition = new FileSet {
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(3::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def belowBoundsSplitPosition = new FileSet {
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(0::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def duplicatedSplitPosition = new FileSet {
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(1::1::Nil)))
  
  @Test(expected=classOf[RefactoringException])
  def tooManySplitPositions = new FileSet {
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """ becomes
    """
      package unorderedSplitPositions
      class Foo {
        def /*(*/add/*)*/(first: Int, second: Int, third: Int) = first + second + third
      }
    """
  } applyRefactoring(curryMethod(List(1::Nil, 1::Nil)))
  
}