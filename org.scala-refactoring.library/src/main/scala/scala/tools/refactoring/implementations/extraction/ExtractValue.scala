package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess

/**
 * Introduces a new val definition for a piece of code.
 */
abstract class ExtractValue extends ExtractionRefactoring with CompilerAccess {
  import global._

  case class PreparationResult(
    selection: Selection,
    possibleExtractions: List[Extraction])

  def prepare(s: Selection) = {
    for {
      selection <- prepareExtractionOfExpressions(s).right
      scopes <- prepareExtractions(
          selection,
          useDefaultInsertionPositions,
          Extraction.hasNoUndefinedDependencies).right
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
        params.selectedExtraction.outboundLocalDeps)
    val transformations = params.selectedExtraction.extractionTransformations(abstraction.call, abstraction.abstraction)

    Right(transformFile(s.file, transformations))
  }
}