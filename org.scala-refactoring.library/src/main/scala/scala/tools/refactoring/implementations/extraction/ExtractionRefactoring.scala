package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.ReplaceableSelections
import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.analysis.TreeAnalysis

trait ExtractionRefactoring extends MultiStageRefactoring with CompilerAccess with ExtractionScopes with Abstractions with InsertionPoints {
  import global._

  val useDefaultInsertionPoints = (s: Selection) =>
    s.beforeSelectionInBlock orElse
      s.afterSelectionInTemplate orElse
      atBeginningOfDefDef orElse
      atBeginningOfFunction

  def prepareValueExpressionsExtraction(s: Selection): Either[PreparationError, Selection] =
    if (s.definesNonLocal)
      Left(PreparationError("Cannot replace selection that defines non local fields."))
    else if (s.definesNonValue)
      Left(PreparationError("Cannot replace selection that defines non value symbols."))
    else
      Right(s)

  def prepareExtractionScopes(s: Selection,
    ip: Selection => InsertionPoint = useDefaultInsertionPoints,
    f: ExtractionScope.Filter = ExtractionScope.allScopes): Either[PreparationError, List[ExtractionScope]] = {
    val scopes = collectExtractionScopes(s, ip(s), f)
    if (scopes.isEmpty)
      Left(PreparationError("No position to insert extraction found."))
    else
      Right(scopes)
  }
}