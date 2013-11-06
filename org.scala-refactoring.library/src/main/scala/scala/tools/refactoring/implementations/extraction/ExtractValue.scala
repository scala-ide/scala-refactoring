package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess

/**
 * Introduces a new val definition for a piece of code.
 */
abstract class ExtractValue extends ExtractionRefactoring with CompilerAccess {
  import global._

  case class PreparationResult(
    selection: Selection,
    potentialScopes: List[ExtractionScope])

  def prepare(s: Selection) = {
    for {
      selection <- prepareExtractionOfExpressions(s).right
      scopes <- prepareExtractionScopes(
          selection,
          useDefaultInsertionPositions,
          ExtractionScope.hasNoUndefinedDependencies).right
    } yield {
      PreparationResult(selection, scopes)
    }
  }

  case class RefactoringParameters(
    name: String,
    selectedScope: ExtractionScope)

  def perform(s: Selection, preparation: PreparationResult, params: RefactoringParameters) = {
    val abstraction = ValueAbstraction(params.name, preparation.selection, params.selectedScope)
    val transformations = params.selectedScope.extractionTransformations(abstraction.call, abstraction.abstraction)

    Right(transformFile(s.file, transformations))
  }
}