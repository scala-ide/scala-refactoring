package scala.tools.refactoring.implementations.extraction

abstract class ExtractMethod extends ExtractionRefactoring {
  import global._

  case class PreparationResult(
    selection: Selection,
    possibleExtractions: List[Extraction])

  def prepare(s: Selection) = {
    for {
      selection <- ensureExpressionsSelected(s).right
      scopes <- collectExtractions(selection, defaultInsertionPositionFor(selection)).right
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
      params.selectedExtraction.undefinedDependencies union params.selectedParameters,
      preparation.selection.outboundLocalDeps)
    val transformations = params.selectedExtraction.extractionTransformations(abstraction.call, abstraction.abstraction)

    Right(transformFile(s.file, transformations))
  }
}