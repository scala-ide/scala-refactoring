package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess

abstract class ExtractMethod extends ExtractionRefactoring with CompilerAccess {
  import global._

  case class PreparationResult(
    selection: Selection,
    possibleExtractions: List[Extraction])

  def prepare(s: Selection) = {
    for {
      selection <- prepareExtractionOfExpressions(s).right
      scopes <- prepareExtractions(selection).right
    } yield {
      PreparationResult(selection, scopes)
    }
  }

  case class RefactoringParameters(
    name: String,
    selectedExtraction: Extraction,
    selectedParameters: List[Symbol])

  def perform(s: Selection, preparation: PreparationResult, params: RefactoringParameters) = {
    val abstraction = MethodAbstraction(
      params.name,
      preparation.selection,
      params.selectedParameters union params.selectedExtraction.undefinedDependencies,
      params.selectedExtraction.outboundLocalDeps)
    val transformations = params.selectedExtraction.extractionTransformations(abstraction.call, abstraction.abstraction)

    Right(transformFile(s.file, transformations))
  }
}