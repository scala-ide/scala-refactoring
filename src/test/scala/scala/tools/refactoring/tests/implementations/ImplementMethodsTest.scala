package scala.tools.refactoring
package tests.implementations

import scala.tools.refactoring.implementations.ImplementMethods
import scala.tools.refactoring.tests.util.{TestHelper, TestRefactoring}

class ImplementMethodsTest extends TestHelper with TestRefactoring {
  outer =>

  val fromSource =
    """
      |package implementMethods
      |
      |trait T {
      |  def f(x: Int): String
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |
      |  def g(x: Int): Int = 1
      |
      |}
    """.stripMargin

  val toSource =
    """
      |package implementMethods
      |
      |trait T {
      |  def f(x: Int): String
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |
      |  def g(x: Int): Int = 1
      |
      |  def f(x: Int): String = {
      |    ???
      |  }
      |
      |}
    """.stripMargin

  def implementMethods(pro: FileSet) = new TestRefactoringImpl(pro) {
    override val refactoring = new ImplementMethods with TestProjectIndex
    val changes = performRefactoring(())
  }.changes

  @Test
  def doRefactor() = new FileSet() {
    fromSource becomes toSource
  } applyRefactoring implementMethods

}
