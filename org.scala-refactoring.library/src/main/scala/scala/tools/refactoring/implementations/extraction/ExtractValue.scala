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
      selection <- prepareReplacementByValueAbstraction(s.expand).right
      scopes <- prepareExtractionScopes(selection, hasNoUndefinedDependencies).right
    } yield {
      PreparationResult(selection, scopes)
    }
  }

  case class RefactoringParameters(
    name: String,
    selectedScope: ExtractionScope)

  def perform(s: Selection, preparation: PreparationResult, params: RefactoringParameters) = {
    val abstraction = mkValDef(params.name, mkBlock(preparation.selection.selectedTopLevelTrees))
    val call = Ident(params.name)

    val trans =
      params.selectedScope.insert(abstraction) &>
        preparation.selection.replaceBy(call)

    Right(transformFile(s.file, trans))
  }
}