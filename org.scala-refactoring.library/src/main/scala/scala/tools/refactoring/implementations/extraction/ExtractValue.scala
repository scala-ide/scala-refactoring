package scala.tools.refactoring.implementations.extraction

/**
 * Introduces a new val definition for a piece of code.
 */
abstract class ExtractValue extends ExtractionRefactoring {
  import global._

  case class PreparationResult(
    selection: Selection,
    possibleExtractions: List[Extraction])

  def prepare(s: Selection) = {
    for {
      selection <- ensureExpressionsSelected(s).right
      allScopes <- collectExtractions(
        selection,
        defaultInsertionPositionFor(selection)).right
      scopes <- ensureNoUndefinedDependencies(allScopes).right
    } yield {
      PreparationResult(selection, scopes)
    }
  }

  case class RefactoringParameters(
    name: String,
    selectedExtraction: Extraction)

  def perform(s: Selection, preparation: PreparationResult, params: RefactoringParameters) = {
    val abstraction = ValueAbstraction(
      params.name,
      preparation.selection,
      preparation.selection.outboundLocalDeps)
    val transformations = params.selectedExtraction.extractionTransformations(abstraction.call, abstraction.abstraction)

    Right(transformFile(s.file, transformations))
  }
}