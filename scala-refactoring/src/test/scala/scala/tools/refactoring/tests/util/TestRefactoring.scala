/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package tests.util

import analysis.GlobalIndexes
import common.Change
import org.junit.Assert._

trait TestRefactoring {
  
  self: TestHelper =>
   
  abstract class TestRefactoringImpl(project: FileSet) {
      
    val refactoring: MultiStageRefactoring
    
    @Deprecated
    def doIt(expected: String, parameters: refactoring.RefactoringParameters) = {
      val result = performRefactoring(parameters)
      assertEquals(expected, Change.applyChanges(result, project.sources.head))
    }
    
    def performRefactoring(parameters: refactoring.RefactoringParameters): List[Change] = {

      val selection = refactoring.FileSelection(project.selection.file, project.selection.pos.start, project.selection.pos.end)
      
      refactoring.prepare(selection) match {
        case Right(prepare) =>
          refactoring.perform(selection, prepare, parameters) match {
            case Right(modifications) => refactoring.refactor(modifications)
            case Left(error) => Predef.error(error.cause)
          }
        case Left(error) => Predef.error(error.cause)
      }
    }
  }
}
