package scala.tools.refactoring
package tests.implementations

import common.Change
import implementations.AddMethod
import org.junit.Assert.assertEquals
import tests.util.TestHelper
import scala.tools.refactoring.implementations._

import language.reflectiveCalls

class AddMethodTest extends TestHelper {
  outer =>

  def addMethod(className: String, methodName: String, parameters: List[List[(String, String)]], returnType: Option[String], target: AddMethodTarget, src: String, expected: String) = {
    val refactoring = new AddMethod with SilentTracing {
      val global = outer.global
      val file = addToCompiler(randomFileName(), src)
      val change = global.ask(() => addMethod(file, className, methodName, parameters, returnType, target))
    }
    assertEquals(expected, Change.applyChanges(refactoring.change, src))
  }

  @Test
  def addMethodToObject = {
    addMethod("Main", "method", Nil, None, AddToObject, """
class Main
object Main {

}""",
      """
class Main
object Main {
  def method = {
    ???
  }
}""")
  }

  @Test
  def addMethodToClass = {
    addMethod("Main", "method", Nil, None, AddToClass, """
object Main
class Main {
  def existingMethod = "this is an existing method"
}""",
      """
object Main
class Main {
  def existingMethod = "this is an existing method"

  def method = {
    ???
  }
}""")
  }

  @Test
  def addMethodWithParametersAndReturnType = {
    addMethod("Main", "method", List(List("a" -> "Any", "b" -> "Int"), List("c" -> "Double")), Some("String"), AddToClass, "class Main",
      """class Main {
  def method(a: Any, b: Int)(c: Double): String = {
    ???
  }
}""")
  }

  @Test
  def addMethodToInnerClass = {
    addMethod("Inner", "method", Nil, None, AddToClass, """
class Main {
  class Inner
}""",
      """
class Main {
  class Inner {
    def method = {
      ???
    }
  }
}""")
  }

  @Test
  def addMethodToCaseClass = {
    addMethod("Main", "method", Nil, None, AddToClass, """
case class Main""",
      """
case class Main {
  def method = {
    ???
  }
}""")
  }

  @Test
  def addMethodToTrait = {
    addMethod("Main", "method", Nil, None, AddToClass, """
trait Main""",
      """
trait Main {
  def method = {
    ???
  }
}""")
  }

  @Test
  def addMethodByClosestPosition = {
    addMethod("Main", "method", Nil, None, AddToClosest(30), """
class Main {

}
object Main {

}""",
      """
class Main {

}
object Main {
  def method = {
    ???
  }
}""")
  }
}