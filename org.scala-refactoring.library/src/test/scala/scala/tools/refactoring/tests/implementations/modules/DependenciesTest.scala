package scala.tools.refactoring.tests.implementations.modules

import scala.tools.refactoring.implementations.modules.Dependencies
import scala.tools.refactoring.implementations.modules.ExtractionSources
import scala.tools.refactoring.implementations.modules.InsertionPoints
import org.junit.Assert._

class DependenciesTest extends TestModule {
  def valueDependencies(file: String) = {
    val fs = new FileSet { file becomes "" }
    new ModuleTest(fs) with Dependencies.Values with ExtractionSources.FromExpressions with InsertionPoints.BeforeSelectionInAnyScope {}
  }

  @Test
  def inboundValueDeps = {
    val refactoring = valueDependencies("""
    object Demo{
      val a = 1
      val b = 2
      def c = {
        /*(*/a * b/*)*/
      }
    }
    """)

    val objectScope = refactoring.scopes(0)
    assertEquals(0, refactoring.undefinedDepsInScope(objectScope).length)
    assertEquals(2, refactoring.definedDepsInScope(objectScope).length)

    val defScope = refactoring.scopes(1)
    assertEquals(0, refactoring.undefinedDepsInScope(defScope).length)
    assertEquals(2, refactoring.definedDepsInScope(defScope).length)
  }

  @Test
  def undefinedInboundValueDeps = {
    val refactoring = valueDependencies("""
    object Demo{
      val a = 1
      def b = {
        val c = 2
        /*(*/a * c/*)*/
      }
    }
    """)

    val objectScope = refactoring.scopes(0)
    assertEquals(1, refactoring.undefinedDepsInScope(objectScope).length)
    assertEquals(1, refactoring.definedDepsInScope(objectScope).length)

    val defScope = refactoring.scopes(1)
    assertEquals(0, refactoring.undefinedDepsInScope(defScope).length)
    assertEquals(2, refactoring.definedDepsInScope(defScope).length)
  }

  @Test
  def inboundMembers = {
    val refactoring = valueDependencies("""
    object Demo{
      def b = {
        /*(*/c/*)*/
      }
      val c = 2
    }
    """)

    val objectScope = refactoring.scopes(0)
    assertEquals(0, refactoring.undefinedDepsInScope(objectScope).length)
    assertEquals(1, refactoring.definedDepsInScope(objectScope).length)

    val defScope = refactoring.scopes(1)
    assertEquals(0, refactoring.undefinedDepsInScope(defScope).length)
    assertEquals(1, refactoring.definedDepsInScope(defScope).length)
  }

  @Test
  def noOutboundDeps = {
    val refactoring = valueDependencies("""
    object Demo{
      val a = 1
      def b = {
        /*(*/a/*)*/
      }
    }
    """)

    assertEquals(0, refactoring.outboundDeps.length)
  }

  @Test
  def outboundDeps = global.ask{ () =>
    val refactoring = valueDependencies("""
    object Demo{
      def a = {
        /*(*/val b = 1
        val c = 2/*)*/
        b
      }
    }
    """)

    assertEquals(1, refactoring.outboundDeps.length)
  }
}