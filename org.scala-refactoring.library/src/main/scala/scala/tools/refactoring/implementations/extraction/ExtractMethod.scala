package scala.tools.refactoring.implementations.extraction

abstract class ExtractMethod extends ExtractionRefactoring {
  import global._

  type E = MethodExtraction
  
  val collector = MethodExtraction
  
  case class RefactoringParameters(selectedExtraction: E, name: String, selectedParameters: List[Symbol])

  def perform(s: Selection, prepared: PreparationResult, params: RefactoringParameters) = {
    val transformations = params.selectedExtraction.perform(params.name, params.selectedParameters)
    Right(transformFile(s.file, transformations))
  }
}