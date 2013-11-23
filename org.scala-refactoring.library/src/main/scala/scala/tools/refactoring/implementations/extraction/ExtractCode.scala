package scala.tools.refactoring.implementations.extraction

abstract class ExtractCode extends ExtractionRefactoring {
  import global._

  type E = Extraction

  val collector = AutoExtraction

  case class RefactoringParameters(
    selectedExtraction: E,
    name: String)

  def perform(s: Selection, prepared: PreparationResult, params: RefactoringParameters) =
    perform(params.selectedExtraction, params.name)
}