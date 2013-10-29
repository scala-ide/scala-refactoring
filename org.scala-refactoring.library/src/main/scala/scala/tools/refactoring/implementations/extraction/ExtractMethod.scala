package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.implementations.modules.Dependencies
import scala.tools.refactoring.implementations.modules.InsertionPoints
import scala.tools.refactoring.implementations.modules.ExtractionSources
import scala.tools.refactoring.Refactoring

abstract class ExtractMethod extends Refactoring
  with ExtractionTargets.NewDef
  with Dependencies.Values
  with InsertionPoints.AfterSelectionInTemplate
  with ExtractionSources.FromExpressions {
  import global._

  def extract(scope: Tree, name: String, selectedParams: List[Symbol]) = {
    val params = undefinedDepsInScope(scope) union selectedParams
    val defDef = abstraction(scope, name, params, outboundDeps, extractedCode)
    val callToDefDef = call(name, params, outboundDeps)

    val insertion = insert(defDef)(scope).get
    val replacement = replaceBy(callToDefDef)(scope).get
    refactor(insertion :: replacement :: Nil)
  }
}