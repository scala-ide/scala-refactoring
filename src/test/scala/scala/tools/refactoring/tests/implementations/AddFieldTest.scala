package scala.tools.refactoring
package tests.implementations

import common.Change
import implementations.AddMethod
import org.junit.Assert.assertEquals
import tests.util.TestHelper
import scala.tools.refactoring.implementations._

import language.reflectiveCalls

class AddFieldTest extends TestHelper {
  outer =>

  def addField(className: String, valName: String, isVar: Boolean, returnType: Option[String], target: AddMethodTarget, src: String, expected: String) = {
    global.ask { () =>
      val refactoring = new AddField {
        val global = outer.global
        val file = addToCompiler(randomFileName(), src)
        val change = addField(file, className, valName, isVar, returnType, target)
      }
      assertEquals(expected, Change.applyChanges(refactoring.change, src))
    }
  }

  @Test
  def addValToObject() = {
    addField("Main", "field", isVar = false, Option("Any"), AddToObject, """
class Main
object Main {

}""",
      """
class Main
object Main {
  val field: Any = ???
}""")
  }

  @Test
  def addValToObject2() = {
    addField("Main", "*", isVar = false, None, AddToObject, """
class Main
object Main {

}""",
      """
class Main
object Main {
  val * = ???
}""")
  }

  @Test
  def addVarToObject() = {
    addField("Main", "field", isVar = true, Option("Any"), AddToObject, """
class Main
object Main {

}""",
      """
class Main
object Main {
  var field: Any = ???
}""")
  }

  @Test
  def addVarToObject2() = {
    addField("Main", "*", isVar = true, None, AddToObject, """
class Main
object Main {

}""",
      """
class Main
object Main {
  var * = ???
}""")
  }

  @Test
  def addValToClass() = {
    addField("Main", "field", isVar = false, Option("Any"), AddToClass, """
object Main
class Main {
  def existingMethod = "this is an existing method"
}""",
      """
object Main
class Main {
  def existingMethod = "this is an existing method"

  val field: Any = ???
}""")
  }

  @Test
  def addValToInnerClass() = {
    addField("Inner", "field", isVar = false, Option("Any"), AddToClass, """
class Main {
  class Inner
}""",
      """
class Main {
  class Inner {
    val field: Any = ???
  }
}""")
  }

  @Test
  def addValToCaseClass() = {
    addField("Main", "field", isVar = false, Option("Any"), AddToClass, """
case class Main""",
      """
case class Main {
  val field: Any = ???
}""")
  }

  @Test
  def addValToTrait() = {
    addField("Main", "field", isVar = false, Option("Any"), AddToClass, """
trait Main""",
      """
trait Main {
  val field: Any = ???
}""")
  }

  @Test
  def addValByClosestPosition() = {
    addField("Main", "field", isVar = false, Option("Any"), AddToClosest(30), """
class Main {

}
object Main {

}""",
      """
class Main {

}
object Main {
  val field: Any = ???
}""")
  }

  @Test
  def addVarByClosestPosition() = {
    addField("Main", "field", isVar = true, Option("Any"), AddToClosest(30), """
class Main {

}
object Main {

}""",
      """
class Main {

}
object Main {
  var field: Any = ???
}""")
  }
}
