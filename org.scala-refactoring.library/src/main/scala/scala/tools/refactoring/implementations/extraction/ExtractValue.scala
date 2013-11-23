package scala.tools.refactoring.implementations.extraction

/**
 * Extracts one or more expressions into a new val definition.
 */
trait ExtractValue extends ExtractionRefactoring {
  import global._

  type E = ValueExtraction
  
  val collector = ValueExtraction

  case class RefactoringParameters(
    selectedExtraction: E,
    name: String)

  def perform(s: Selection, prepared: PreparationResult, params: RefactoringParameters) =
    perform(params.selectedExtraction, params.name)
}