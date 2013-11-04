package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.analysis.Indexes

trait Abstractions extends ExtractionScopes with TreeAnalysis with Indexes { self: CompilerAccess =>
  import global._

  trait Abstraction {
    val call: Tree
    val abstraction: Tree

    val selection: Selection
    val extractionScope: ExtractionScope

    val outboundDeps = outboundLocalDependencies(selection)

    lazy val extractionTransformation = {
      selection.replaceBy(call) &>
        extractionScope.insert(abstraction)
    }
  }

  case class ValueAbstraction(
    name: String,
    selection: Selection,
    extractionScope: ExtractionScope) extends Abstraction {

    val call = mkCallValDef(name, outboundDeps)

    val returnStatements =
      if (outboundDeps.isEmpty) Nil
      else mkReturn(outboundDeps) :: Nil

    val statements = selection.selectedTopLevelTrees ::: returnStatements

    val abstraction = statements match {
      case expr :: Nil => mkValDef(name, expr)
      case stmts => mkValDef(name, mkBlock(stmts))
    }
  }

  case class MethodAbstraction(
    name: String,
    selection: Selection,
    extractionScope: ExtractionScope,
    selectedParameters: List[Symbol]) extends Abstraction {

    val parameters = (extractionScope.undefinedDependencies union selectedParameters) :: Nil

    val call = mkCallDefDef(name, parameters, outboundDeps)

    val returnStatements =
      if (outboundDeps.isEmpty) Nil
      else mkReturn(outboundDeps) :: Nil

    val statements = selection.selectedTopLevelTrees ::: returnStatements

    val abstraction = mkDefDef(NoMods, name, parameters, statements)
  }
}