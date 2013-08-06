/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.util

import analysis.GlobalIndexes
import common.Change
import org.junit.Assert._
import scala.tools.refactoring.common.CompilerAccess
import common.InteractiveScalaCompiler
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.common.Selections
import org.junit.Before

trait TestRefactoring extends TestHelper {

  @Before
  def cleanup() = resetPresentationCompiler()

  class PreparationException(cause: String) extends Exception(cause)
  class RefactoringException(cause: String) extends Exception(cause)

  abstract class TestRefactoringImpl(project: FileSet) {

    trait TestProjectIndex extends GlobalIndexes {
      this: Refactoring =>

      val global = TestRefactoring.this.global

      override val index = global.ask { () =>

        val trees = project.sources map (x => addToCompiler(project.fileName(x), x)) map (global.unitOfFile(_).body)

        val cuIndexes = trees map (_.pos.source.file) map { file =>
          global.unitOfFile(file).body
        } map CompilationUnitIndex.apply
        GlobalIndex(cuIndexes)
      }
    }

    val refactoring: MultiStageRefactoring with InteractiveScalaCompiler

    def preparationResult = global.ask { () =>
      refactoring.prepare(selection(refactoring, project))
    }

    def performRefactoring(parameters: refactoring.RefactoringParameters): List[Change] = global.ask { () =>
      preparationResult match {
        case Right(prepare) =>
          refactoring.perform(selection(refactoring, project), prepare, parameters) match {
            case Right(modifications) => modifications
            case Left(error) => throw new RefactoringException(error.cause)
          }
        case Left(error) => throw new PreparationException(error.cause)
      }
    }
  }
}
