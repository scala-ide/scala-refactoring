package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess

abstract class ExtractMethod extends ExtractionRefactoring with CompilerAccess {
  import global._

  case class PreparationResult(
    selection: Selection,
    potentialScopes: List[ExtractionScope])

  def prepare(s: Selection) = {
    for {
      selection <- prepareExtractionOfExpressions(s).right
      scopes <- prepareExtractionScopes(selection).right
    } yield {
      PreparationResult(selection, scopes)
    }
  }

  case class RefactoringParameters(
    name: String,
    selectedScope: ExtractionScope,
    selectedParameters: List[Symbol])

  def perform(s: Selection, preparation: PreparationResult, params: RefactoringParameters) = {
    val abstraction = MethodAbstraction(params.name, preparation.selection, params.selectedScope, params.selectedParameters)
    val transformations = params.selectedScope.extractionTransformations(abstraction.call, abstraction.abstraction)

    Right(transformFile(s.file, transformations))
  }
}