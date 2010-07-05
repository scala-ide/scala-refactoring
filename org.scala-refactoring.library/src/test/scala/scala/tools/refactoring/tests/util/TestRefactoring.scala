/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.util

import analysis.GlobalIndexes
import common.Change
import org.junit.Assert._

trait TestRefactoring extends TestHelper {
     
  abstract class TestRefactoringImpl(project: FileSet) {
      
    val refactoring: MultiStageRefactoring

    def performRefactoring(parameters: refactoring.RefactoringParameters): List[Change] = {

      val selection = refactoring.FileSelection(project.selection.file, project.selection.pos.start, project.selection.pos.end)
      
      refactoring.prepare(selection) match {
        case Right(prepare) =>
          refactoring.perform(selection, prepare, parameters) match {
            case Right(modifications) => modifications
            case Left(error) => Predef.error(error.cause)
          }
        case Left(error) => Predef.error(error.cause)
      }
    }
  }
}
