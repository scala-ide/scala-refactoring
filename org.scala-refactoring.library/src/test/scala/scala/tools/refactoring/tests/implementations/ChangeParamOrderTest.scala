package scala.tools.refactoring
package tests.implementations

import implementations.ChangeParamOrder
import tests.util.TestHelper
import tests.util.TestRefactoring
import org.junit.Ignore

class ChangeParamOrderTest extends TestHelper with TestRefactoring {

  outer =>
    
  def changeParamOrder(permutations: List[List[Int]])(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ChangeParamOrder with SilentTracing {
      val global = outer.global
    }
    val changes = performRefactoring(permutations)
  }.changes
  
  @Test
  // TODO: resolve pretty print issues
  def exchangeTwoParams = new FileSet {
    """
      package exchangeTwoParamsTest
      class Foo {
        def /*(*/twoParams/*)*/(first: Int, second: Int) = second + first
      }
    """ becomes
    """
      package exchangeTwoParamsTest
      class Foo {
        def /*(*/twoParams/*)*/( second: Int,first: Int) = second + first
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::Nil)))
  
  @Test
  // TODO resolve issue with last parameter moving
  def multipleParameterLists = new FileSet {
    """
      package multipleParameterLists
      class Foo {
        def /*(*/multipleParamLists/*)*/[A](first: Int, second: Int, third: Int)(flag: Boolean, f: Int => String, foo: A) = flag match {
          case true => f(first) + f(second) + third
          case false => third
        }
      }
    """ becomes
    """
      package multipleParameterLists
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
      package methodCall
      class Defining {
        def /*(*/method/*)*/(first: Int, second: String, third: Int) = second + first 
      }
      class Calling {
        val defining = new Defining
        val result = defining.method(5, "asdf", 3)
      }
    """ becomes
    """
      package methodCall
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
      package methodCallMultipleParamLists
      class Defining {
        def /*(*/method/*)*/(first: Int, second: String, third: Int)(a: Int, b: String, c: Int, d: Int) = second + first 
      }
      class Calling {
        val defining = new Defining
        val result = defining.method(5, "asdf", 3)(1, 2, 3, 4)
      }
    """ becomes
    """
      package methodCallMultipleParamLists
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
  def curriedMethodCall = new FileSet {
    """
      package methodCallMultipleParamLists
      class Defining {
        def /*(*/method/*)*/(first: Int, second: String, third: Int)(a: Int, b: String, c: Int, d: Int) = second + first 
      }
      class Currying {
        val defining = new Defining
        val curried = defining.method(5, "asdf", 3)(_, _, _, _)
      }
    """ becomes
    """
      package methodCallMultipleParamLists
      class Defining {
        def /*(*/method/*)*/( second: String,first: Int, third: Int)( b: String, c: Int,a: Int, d: Int) = second + first 
      }
      class Currying {
        val defining = new Defining
        val curried = defining.method( "asdf",5, 3)( _, _,_, _)
      }
    """
  } applyRefactoring(changeParamOrder(List(1::0::2::Nil, 1::2::0::3::Nil)))
  
  @Test
  def dummy = {
    val tree = treeFrom {
    """
      class Defining {
        def /*(*/method/*)*/(first: Int, second: String, third: Int)(a: Int) = second + first 
      }
      class Calling {
        val defining = new Defining
        val result = defining.method(5, "asdf", 3)(5/17)
      }
    """
    }
    println("foo")
  }
  
}