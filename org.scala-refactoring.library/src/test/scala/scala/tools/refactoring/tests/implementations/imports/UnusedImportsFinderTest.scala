/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.UnusedImportsFinder
import tests.util.TestHelper

class UnusedImportsFinderTest extends TestHelper {
  outer =>

  def findUnusedImports(expected: String, src: String) {

   val unuseds = global.ask { () =>
      new UnusedImportsFinder {

        val global = outer.global

        val unit = global.unitOfFile(addToCompiler(randomFileName(), src))

        def compilationUnitOfFile(f: AbstractFile) = Some(unit)

        val unuseds = findUnusedImports(unit)
      }.unuseds
    }

   org.junit.Assert.assertEquals(expected, unuseds.mkString(", "))
  }

  @Test
  def simpleUnusedType() = findUnusedImports(
    "(ListBuffer,2)",
    """
      import scala.collection.mutable.ListBuffer

      object Main {val s: String = "" }
    """
  )

  @Test
  def typeIsUsedAsVal() = findUnusedImports(
    "",
    """
      import scala.collection.mutable.ListBuffer

      object Main {val s = new ListBuffer[Int] }
    """
  )

  @Test
  def typeIsImportedFrom() = findUnusedImports(
    "",
    """
      class Forest {
        class Tree
      }

      class UsesTrees {
        val forest = new Forest
        import forest._
        val x = new Tree
      }
    """
  )

  @Test
  def wildcardImports() = findUnusedImports(
    "",
    """
      import scala.util.control.Exception._

      class UsesTrees {
        val plugin = ScalaPlugin.plugin
        import plugin._
        ()
      }
    """
  )

  @Test
  def wildcardImportsFromValsAreIgnored() = findUnusedImports(
    "",
    """
      object ScalaPlugin {
        var plugin: String = _
      }

      class UsesTrees {
        val plugin = ScalaPlugin.plugin
        import plugin._
        ()
      }
    """
  )

  @Test
  def importFromJavaClass() = findUnusedImports(
    "",
    """
      import java.util.Date

      object ScalaPlugin {
        import Date._
        val max = parse(null)
      }
    """
  )

  @Test
  def ignoreImportIsNeverunused() = findUnusedImports(
    "",
    """
      import java.util.{Date => _, _}

      object NoDate {
        var x: Stack[Int] = null
      }
    """
  )

  // more tests are in organize imports
}
