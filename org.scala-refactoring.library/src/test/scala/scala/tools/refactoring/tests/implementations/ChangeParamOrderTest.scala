package scala.tools.refactoring
package tests.implementations

import implementations.ChangeParamOrder
import tests.util.TestHelper
import tests.util.TestRefactoring
import org.junit.Ignore

class ChangeParamOrderTest extends TestHelper with TestRefactoring {

  outer =>
    
  def changeParamOrder(permutations: List[List[Int]])(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ChangeParamOrder with SilentTracing with GlobalIndexes {
      val global = outer.global
      val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
      val index = GlobalIndex(cuIndexes)
    }
    val changes = performRefactoring(permutations)
  }.changes
  
  @Test
  @Ignore // TODO: resolve pretty print issues
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
        def /*(*/twoParams/*)*/( second: Int,first: Int) = second + first
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
        def /*(*/multipleParamLists/*)*/[A]( second: Int,first: Int, third: Int)( f: Int => String,flag: Boolean, foo: A) = flag match {
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
        def /*(*/method/*)*/( second: String,first: Int, third: Int) = second + first 
      }
      class Calling {
        val defining = new Defining
        val result = defining.method( "asdf",5, 3)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil)))
  
  @Test
  def methodCallMultipleParamLists = new FileSet {
    """
      package changeParamOrder.methodCallMultipleParamLists
      class Defining {
        def /*(*/method/*)*/(first: Int, second: String, third: Int)(a: Int, b: String, c: Int, d: Int) = second + first 
      }
      class Calling {
        val defining = new Defining
        val result = defining.method(5, "asdf", 3)(1, 2, 3, 4)
      }
    """ becomes
    """
      package changeParamOrder.methodCallMultipleParamLists
      class Defining {
        def /*(*/method/*)*/( second: String,first: Int, third: Int)( b: String, c: Int,a: Int, d: Int) = second + first 
      }
      class Calling {
        val defining = new Defining
        val result = defining.method( "asdf",5, 3)( 2, 3,1, 4)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil, 1::2::0::3::Nil)))
  
  @Test
  @Ignore // TODO: fix
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
        def /*(*/method/*)*/( second: String,first: Int, third: Int)( b: String, c: Int,a: Int, d: Int) = second + first 
      }
      class Currying {
        val defining = new Defining
        def curried = defining.method( "asdf",5, 3) _
        def curriedWithArgs(a: Int, b: String, c: Int, d: Int) = defining.method( "a",1, 2)( b, c,a, d)
        def fullyApplied = curried( "asdf", 2,1, 3)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil, 1::2::0::3::Nil)))
  
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
        def /*(*/method/*)*/( b: Int,a: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        override def method( b: Int,a: Int, c: Int) = a*b*c
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
        def method( b: Int,a: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        override def /*(*/method/*)*/( b: Int,a: Int, c: Int) = a*b*c
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
        def /*(*/method/*)*/( b: Int,a: Int, c: Int) = a + b + c
      }

      class Child extends Parent {
        val sum3 = method( 2,1, 3)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil)))
  
}