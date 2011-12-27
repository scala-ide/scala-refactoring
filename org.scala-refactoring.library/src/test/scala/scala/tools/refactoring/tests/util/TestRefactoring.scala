/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.util

import analysis.GlobalIndexes
import common.Change
import org.junit.Assert._

trait TestRefactoring extends TestHelper {
  
  class PreparationException(cause: String) extends Exception(cause)
  class RefactoringException(cause: String) extends Exception(cause)
  
  abstract class TestRefactoringImpl(project: FileSet) {
      
    trait TestProjectIndex extends GlobalIndexes {
      this: Refactoring =>
      
      val global = TestRefactoring.this.global
        
      val index = {
        val cuIndexes = project.trees map (_.pos.source.file) map { file => 
          global.unitOfFile(file).body
        } map CompilationUnitIndex.apply
        GlobalIndex(cuIndexes)
      }      
    }
    
    val refactoring: MultiStageRefactoring

    def performRefactoring(parameters: refactoring.RefactoringParameters): List[Change] = {

      val selection = refactoring.FileSelection(project.selection.file, project.selection.pos.start, project.selection.pos.end)
      
      refactoring.prepare(selection) match {
        case Right(prepare) =>
          refactoring.perform(selection, prepare, parameters) match {
            case Right(modifications) => modifications
            case Left(error) => throw new PreparationException(error.cause)
          }
        case Left(error) => throw new RefactoringException(error.cause)
      }
    }
  }
}
