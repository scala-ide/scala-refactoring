/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.sourcegen

import tests.util.TestHelper
import org.junit.Assert
import org.junit.Assert._
import sourcegen.SourceGenerator
import common.SilentTracing
import common.ConsoleTracing
import scala.tools.refactoring.implementations.OrganizeImports
import scala.tools.refactoring.tests.util.TestRefactoring

import language.reflectiveCalls

class CustomFormattingTest extends TestHelper with TestRefactoring with SourceGenerator with SilentTracing {

  var surroundingImport = ""

  override def spacingAroundMultipleImports = surroundingImport

  abstract class OrganizeImportsRefatoring(pro: FileSet) extends TestRefactoringImpl(pro) {
    val refactoring = new OrganizeImports with SilentTracing {
      val global = CustomFormattingTest.this.global
      override def spacingAroundMultipleImports = surroundingImport
    }
    type RefactoringParameters = refactoring.RefactoringParameters
    val params: RefactoringParameters
    def mkChanges = performRefactoring(params)
  }

  def organize(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters()
  }.mkChanges


  @Test
  @Ignore // TODO sometimes fails on Jenkins, need to investigate
  def testSingleSpace() {

    val ast = treeFrom("""
    import scala.collection.{MapLike, MapProxy}
    """)

    surroundingImport = " "

    assertEquals("""
    import scala.collection.{ MapLike, MapProxy }
    """, createText(ast, Some(ast.pos.source)))
  }

  @Test
  def collapse = {
    surroundingImport = " "

    new FileSet {
      """
        import java.lang.String
        import java.lang.Object

        object Main {val s: String = ""; var o: Object = null}
      """ becomes
      """
        import java.lang.{ Object, String }

        object Main {val s: String = ""; var o: Object = null}
      """
    } applyRefactoring organize
  }
}

