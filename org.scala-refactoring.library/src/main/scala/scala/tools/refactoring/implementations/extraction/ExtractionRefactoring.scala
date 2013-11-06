package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.ReplaceableSelections
import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.analysis.TreeAnalysis

trait ExtractionRefactoring extends MultiStageRefactoring with CompilerAccess with Extractions with Abstractions with InsertionPoints {
  import global._

  /**
   * Creates an insertion position that inserts local values
   * before the selection `s` and class members after the
   * method that contains the selection.
   */
  val useDefaultInsertionPositions = (s: Selection) =>
    s.beforeSelectionInBlock orElse
      s.afterSelectionInTemplate orElse
      atBeginningOfDefDef orElse
      atBeginningOfFunction

  /**
   * Tries to find a valid selection that is replaceable by a expression.
   * Returns either a message why the selection is not valid or a selection
   * that has all required properties.
   */
  def prepareExtractionOfExpressions(s: Selection): Either[PreparationError, Selection] = {
    val expanded = s.expand
    if (expanded.definesNonLocal)
      Left(PreparationError("Cannot extract selection that defines non local fields."))
    else if (expanded.definesNonValue)
      Left(PreparationError("Cannot extract selection that defines non value symbols."))
    else if (expanded.containsImportStatements)
      Left(PreparationError("Cannot extract selection that contains import statements."))
    else
      Right(expanded)
  }

  /**
   * Tries to find possible extractions that matches the given insertion position
   * and fulfill all filter predicates.
   */
  def prepareExtractions(s: Selection, 
    ip: Selection => InsertionPosition = useDefaultInsertionPositions, 
    f: Extraction.Filter = Extraction.allScopes): Either[PreparationError, List[Extraction]] = {
    val scopes = collectExtractions(s, ip(s), f)
    if (scopes.isEmpty)
      Left(PreparationError("No position to insert extraction found."))
    else
      Right(scopes)
  }
}