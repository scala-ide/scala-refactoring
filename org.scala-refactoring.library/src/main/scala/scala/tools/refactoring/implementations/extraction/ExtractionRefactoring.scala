package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.ReplaceableSelections
import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.analysis.TreeAnalysis

trait ExtractionRefactoring extends MultiStageRefactoring with CompilerAccess with ExtractionScopes {
  import global._

  def prepareReplacementByValueAbstraction(s: Selection): Either[PreparationError, Selection] =
    if (s.definesNonLocal)
      Left(PreparationError("Cannot replace selection that defines non local fields."))
    else if (s.definesNonValue)
      Left(PreparationError("Cannot replace selection that defines non value symbols."))
    else
      Right(s)
      
  def prepareExtractionScopes(s: Selection): Either[PreparationError, List[ExtractionScope]] = ???
}