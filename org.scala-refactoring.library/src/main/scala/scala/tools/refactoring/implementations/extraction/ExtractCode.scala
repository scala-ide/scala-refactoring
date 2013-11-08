package scala.tools.refactoring.implementations.extraction

abstract class ExtractCode extends ExtractionRefactoring {
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
    val ps = params.selectedParameters union params.selectedExtraction.undefinedDependencies
    val abstraction =
      if (ps.isEmpty)
        ValueAbstraction(params.name, preparation.selection, preparation.selection.outboundLocalDeps)
      else
        MethodAbstraction(params.name, preparation.selection, ps, preparation.selection.outboundLocalDeps)

    val transformations = params.selectedExtraction.extractionTransformations(abstraction.call, abstraction.abstraction)

    Right(transformFile(s.file, transformations))
  }
}