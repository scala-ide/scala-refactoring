package scala.tools.refactoring
package tests.implementations

import scala.tools.refactoring.implementations.ImplementMethods
import scala.tools.refactoring.tests.util.{TestHelper, TestRefactoring}

class ImplementMethodsTest extends TestHelper with TestRefactoring {
  outer =>

  def implementMethods(pro: FileSet) = new TestRefactoringImpl(pro) {
    override val refactoring = new ImplementMethods with TestProjectIndex
    val changes = performRefactoring(())
  }.changes

  @Test
  def implementMethodFromFirstMixing() = new FileSet() {
    """
      |package implementMethods
      |
      |trait T {
      |  def f(x: Int): String
      |}
      |
      |trait S {
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/T/*)*/ with S {
      |  val x: Int = ???
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait T {
      |  def f(x: Int): String
      |}
      |
      |trait S {
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/T/*)*/ with S {
      |  val x: Int = ???
      |
      |  def f(x: Int): String = {
      |    ???
      |  }
      |}
    """.stripMargin
  } applyRefactoring implementMethods

  @Test
  def implementMethodFromExtendedClass() = new FileSet() {
    """
      |package implementMethods
      |
      |abstract class C {
      |  def f(x: Int): String
      |}
      |
      |object Obj extends /*(*/C/*)*/ {
      |  val x: Int = ???
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |abstract class C {
      |  def f(x: Int): String
      |}
      |
      |object Obj extends /*(*/C/*)*/ {
      |  val x: Int = ???
      |
      |  def f(x: Int): String = {
      |    ???
      |  }
      |}
    """.stripMargin
  } applyRefactoring implementMethods

  @Test
  def implementMethodFromSecondMixing() = new FileSet() {
    """
      |package implementMethods
      |
      |trait T {
      |  def f(x: Int): String
      |}
      |
      |trait S {
      |  def g(x: Int): Int
      |}
      |
      |class C extends T with /*(*/S/*)*/ {
      |  val x: Int = ???
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait T {
      |  def f(x: Int): String
      |}
      |
      |trait S {
      |  def g(x: Int): Int
      |}
      |
      |class C extends T with /*(*/S/*)*/ {
      |  val x: Int = ???
      |
      |  def g(x: Int): Int = {
      |    ???
      |  }
      |}
    """.stripMargin
  } applyRefactoring implementMethods

  @Test
  def implementNotImplementedMethods() = new FileSet() {
    """
      |package implementMethods
      |
      |trait T {
      |  def f(x: Int): String
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |
      |  def g(x: Int): Int = 1
      |
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait T {
      |  def f(x: Int): String
      |  def g(x: Int): Int
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
  } applyRefactoring implementMethods

  @Test
  def implementNotImplementedMembers() = new FileSet() {
    """
      |package implementMethods
      |
      |trait T {
      |
      |  val x: Int
      |  val y: Double
      |  val a_longIdentifier: Long
      |
      |  def f(x: Int): String
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |
      |  val y: Double = 42.0
      |  val a_longIdentifier: Long = 42L
      |
      |  def g(x: Int): Int = 1
      |
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait T {
      |
      |  val x: Int
      |  val y: Double
      |  val a_longIdentifier: Long
      |
      |  def f(x: Int): String
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |
      |  val y: Double = 42.0
      |  val a_longIdentifier: Long = 42L
      |
      |  def g(x: Int): Int = 1
      |
      |  val x: Int = ???
      |
      |  def f(x: Int): String = {
      |    ???
      |  }
      |
      |}
    """.stripMargin
  } applyRefactoring implementMethods

  @Test
  def implementMethodsSelectType() = new FileSet() {
    """
      |package implementMethods
      |
      |trait T[T] {
      |  def f[T]: T
      |}
      |
      |class C extends /*(*/T[Int]/*)*/ {
      |
      |  def g(x: Int): Int = ???
      |
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait T[T] {
      |  def f[T]: T
      |}
      |
      |class C extends /*(*/T[Int]/*)*/ {
      |
      |  def g(x: Int): Int = ???
      |
      |  def f[T]: T = {
      |    ???
      |  }
      |
      |}
    """.stripMargin
  } applyRefactoring implementMethods

  @Test
  def implementMethodsSelectKind() = new FileSet() {
    """
      |package implementMethods
      |
      |trait T[T] {
      |  def f[T]: T
      |}
      |
      |class C extends /*(*/T/*)*/[Int] {
      |
      |  def g(x: Int): Int = ???
      |
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait T[T] {
      |  def f[T]: T
      |}
      |
      |class C extends /*(*/T/*)*/[Int] {
      |
      |  def g(x: Int): Int = ???
      |
      |  def f[T]: T = {
      |    ???
      |  }
      |
      |}
    """.stripMargin
  } applyRefactoring implementMethods


  @Test
  def implementMethodFromAncestry() = new FileSet() {
    """
      |package implementMethods
      |
      |trait R {
      |  def k: Unit
      |}
      |
      |trait T {
      |  def f(x: Int): String
      |}
      |
      |trait S extends T {
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/S/*)*/ with R {
      |  val x: Int = ???
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait R {
      |  def k: Unit
      |}
      |
      |trait T {
      |  def f(x: Int): String
      |}
      |
      |trait S extends T {
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/S/*)*/ with R {
      |  val x: Int = ???
      |
      |  def g(x: Int): Int = {
      |    ???
      |  }
      |
      |  def f(x: Int): String = {
      |    ???
      |  }
      |}
    """.stripMargin

  } applyRefactoring implementMethods

  @Test
  def implementMethodFromCyclicAncestry() = new FileSet() {
    """
      |package implementMethods
      |
      |trait R {
      |  def k: Unit
      |}
      |
      |trait T extends R {
      |  def f(x: Int): String
      |}
      |
      |trait S extends T with R {
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/S/*)*/ with R {
      |  val x: Int = ???
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait R {
      |  def k: Unit
      |}
      |
      |trait T extends R {
      |  def f(x: Int): String
      |}
      |
      |trait S extends T with R {
      |  def g(x: Int): Int
      |}
      |
      |object Obj extends /*(*/S/*)*/ with R {
      |  val x: Int = ???
      |
      |  def g(x: Int): Int = {
      |    ???
      |  }
      |
      |  def f(x: Int): String = {
      |    ???
      |  }
      |
      |  def k: Unit = {
      |    ???
      |  }
      |}
    """.stripMargin

  } applyRefactoring implementMethods

  @Test
  def implementTypes() = new FileSet() {
    """
      |package implementMethods
      |
      |trait T {
      |  type S
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |  val x: Int = 3
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait T {
      |  type S
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |  val x: Int = 3
      |
      |  type S = this.type
      |}
    """.stripMargin
  } applyRefactoring implementMethods

  @Test
  def implementTypesSkippingImplemented() = new FileSet() {
    """
      |package implementMethods
      |
      |trait T {
      |  type S
      |  type R
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |  type R = Int
      |  val x: Int = 3
      |}
    """.stripMargin becomes
    """
      |package implementMethods
      |
      |trait T {
      |  type S
      |  type R
      |}
      |
      |object Obj extends /*(*/T/*)*/ {
      |  type R = Int
      |  val x: Int = 3
      |
      |  type S = this.type
      |}
    """.stripMargin
  } applyRefactoring implementMethods

}
