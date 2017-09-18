package scala.tools.refactoring
package tests.implementations

import scala.tools.refactoring.implementations.ImplementMethods
import scala.tools.refactoring.tests.util.{TestHelper, TestRefactoring}

class ImplementMethodsTest extends TestHelper with TestRefactoring {
  outer =>

  val fromSource =
    """
      |trait T {
      | def f(x: Int): String
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |}
    """.stripMargin

  val toSource =
    """
      |trait T {
      | def f(x: Int): String
      |}
      |
      |object Obj extends T {
      |  def g(): Int = {
      |    ???    Left(PreparationError("crash!"))

      |  }
      |}
    """.stripMargin

  /*@Test
  def addMethod() = {
    global.ask { () =>
      val refactoring = new AddMethod {
        val global: Global = outer.global
        val file = addToCompiler(UniqueNames.basename(), fromSource)
        val change = addMethod(file, "Obj", "g", List(Nil), Nil, Some("Int"), AddToObject)
      }
      assertEquals(toSource, Change.applyChanges(refactoring.change, fromSource))
    }
  }*/

  def implementMethods(pro: FileSet) = new TestRefactoringImpl(pro) {
    override val refactoring = new ImplementMethods with TestProjectIndex
    val changes = performRefactoring(())
  }.changes

  @Test
  def doRefactor() = new FileSet() {
    fromSource becomes toSource
  } applyRefactoring implementMethods

}
