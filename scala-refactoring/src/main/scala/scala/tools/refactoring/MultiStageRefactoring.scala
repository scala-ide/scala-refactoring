/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring

abstract class MultiStageRefactoring extends Refactoring {
  
  type PreparationResult
  
  case class PreparationError(val cause: String)
  
  case class RefactoringError(val cause: String)
  
  type RefactoringParameters
 
  def prepare(s: Selection): Either[PreparationError, PreparationResult]
  
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[global.Tree]]

}