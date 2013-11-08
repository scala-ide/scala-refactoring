package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.transformation.TreeTransformations
import scala.reflect.internal.Flags

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

    val abstraction = {
      def symbolToParam(s: Symbol) = {
        /* The type of a symbol referencing class fields is "=> T"
       * and therefore converted to a by name parameter. But in most cases
       * it is preferred to pass it as a by value parameter.
       */
        val tpe = if (s.tpe.toString.startsWith("=>"))
          s.tpe.baseTypeSeq(0)
        else
          s.tpe
        new ValDef(Modifiers(Flags.PARAM), newTermName(s.nameString), TypeTree(tpe), EmptyTree)
      }

      val ps = parameters.map(symbolToParam) :: Nil

      val returnTpe = statements.last match {
        case t: Function if t.pos.isTransparent =>
          TypeTree(t.body.tpe)
        case t =>
          TypeTree(t.tpe)
      }

      DefDef(NoMods withPosition (Flags.METHOD, NoPosition), newTermName(name), Nil, ps, returnTpe, mkBlock(statements))
    }
  }
}