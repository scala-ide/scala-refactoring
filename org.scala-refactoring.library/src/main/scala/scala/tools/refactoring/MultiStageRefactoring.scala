/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring

import common.Change

/**
 * The super class of all refactoring implementations,
 * representing the several phases of the refactoring
 * process.
 */
abstract class MultiStageRefactoring extends Refactoring {

  this: common.CompilerAccess =>

  /**
   * Preparing a refactoring can either return a result
   * or an instance of PreparationError, describing the
   * cause why the refactoring cannot be performed.
   */

  type PreparationResult

  case class PreparationError(cause: String)

  def prepare(s: Selection): Either[PreparationError, PreparationResult]

  /**
   * Refactorings are parameterized by the user, and to keep
   * them stateless, the result of the preparation step needs
   * to be passed to the perform method.
   *
   * The result can either be an error or a list of trees that
   * contain changes.
   */

  type RefactoringParameters

  case class RefactoringError(cause: String)

  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]]

}