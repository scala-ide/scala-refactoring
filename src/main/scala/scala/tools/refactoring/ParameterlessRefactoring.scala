package scala.tools.refactoring

import common.Change

/**
 * A helper trait for refactorings that don't take RefactoringParameters.
 *
 * With this trait, the refactoring can implement the simplified perform
 * method.
 */
trait ParameterlessRefactoring {

  this: MultiStageRefactoring =>

  class RefactoringParameters

  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    perform(selection, prepared)
  }

  def perform(selection: Selection, prepared: PreparationResult): Either[RefactoringError, List[Change]]
}