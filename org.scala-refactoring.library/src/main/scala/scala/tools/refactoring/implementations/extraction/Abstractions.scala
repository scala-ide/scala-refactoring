package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.transformation.TreeTransformations

trait Abstractions extends Selections with TreeFactory with TreeTransformations { self: CompilerAccess =>
  import global._

  trait Abstraction {
    val call: Tree
    val abstraction: Tree
    val selection: Selection
  }

  case class ValueAbstraction(
    name: String,
    selection: Selection,
    outboundDeps: List[Symbol]) extends Abstraction {

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
    parameters: List[Symbol],
    outboundDeps: List[Symbol]) extends Abstraction {
    
    val call = mkCallDefDef(name, parameters :: Nil, outboundDeps)

    val returnStatements =
      if (outboundDeps.isEmpty) Nil
      else mkReturn(outboundDeps) :: Nil

    val statements = selection.selectedTopLevelTrees ::: returnStatements

    val abstraction = mkDefDef(NoMods, name, parameters :: Nil, statements)
  }
}