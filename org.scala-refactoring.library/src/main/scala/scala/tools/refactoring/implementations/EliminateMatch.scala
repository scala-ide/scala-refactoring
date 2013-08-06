/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import tools.nsc.symtab.Flags
import transformation.TreeFactory

abstract class EliminateMatch extends MultiStageRefactoring with common.TreeExtractors with TreeFactory {

  this: common.CompilerAccess =>

  import global._

  object Elimination extends Enumeration {
    val ForEach   = Value("foreach")
    val Map       = Value("map")
    val FlatMap   = Value("flatMap")
    val IsDefined = Value("isDefined")
    val IsEmpty   = Value("isEmpty")
    val Exists    = Value("exists")
    val Forall    = Value("forall")
    val Flatten   = Value("flatten")
    val OrElse    = Value("orElse")
    val GetOrElse = Value("getOrElse")
    val ToList    = Value("toList")
  }

  type PreparationResult = (Elimination.Value, Position, Transformation[Tree, Tree])

  class RefactoringParameters

  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {

    val res = s.findSelectedOfType[Match] map { t => getMatchElimination(t)}

    res getOrElse Left(PreparationError("No elimination candidate found."))
  }

  def getMatchElimination(t: Tree): Either[PreparationError, PreparationResult] = {

    /**
     * When replacing with `map`, we need to remove the explicit `Some` construction
     * in the case body. There are two possible kinds of case bodies: a simple Apply
     * call and a Block that has the Some at its end.
     */
    def replaceWithMap(mtch: Match, param: TermName, someCaseBody: Tree) = {
      transform {
        case `mtch` =>
          someCaseBody match {

            case Apply(_, arg :: Nil) =>
              mkFunctionCallWithFunctionArgument(mtch.selector, "map", param, arg)

            case Block(stmts, Apply(_, arg :: Nil)) =>
              mkFunctionCallWithFunctionArgument(mtch.selector, "map", param, global.Block(stmts, arg))

            case _ => throw new Exception("Please file a bug report.")
          }
      }
    }

    def replaceWithCall(fun: String, mtch: Match, param: TermName, body: Tree) = {
      transform {
        case `mtch` =>
          mkFunctionCallWithFunctionArgument(mtch.selector, fun, param, body)
      }
    }

    def replaceWithExpr(fun: String, mtch: Match, body: Tree) = {
      transform {
        case `mtch` =>
          mkFunctionCallWithZeroArgFunctionArgument(mtch.selector, fun, body)
      }
    }

    def replaceWith(fun: String, mtch: Match) = {
      transform {
        case `mtch` =>
          Select(mtch.selector, newTermName(fun))
      }
    }

    /*
     * A huge thanks to Tony Morris for his scala.Option Cheat Sheet
     */

    Option(t) collect {

      /* foreach */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body, UnitLit())) =>
        (Elimination.ForEach, m.pos, replaceWithCall("foreach", m, name, body))

      /* map */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ (SomeExpr(_) | Block(_, SomeExpr(_))), NoneExpr())) =>
         (Elimination.Map, m.pos, replaceWithMap(m, name, body))

      /* flatMap */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ (HasType("Option" | "Some" | "None")), NoneExpr())) =>
        (Elimination.FlatMap, m.pos, replaceWithCall("flatMap", m, name, body))

      /* isDefined */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(_, Literal(Constant(true)), Literal(Constant(false)))) =>
        (Elimination.IsDefined, m.pos, replaceWith("isDefined", m))

      /* isEmpty */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, Literal(Constant(false)), Literal(Constant(true)))) =>
        (Elimination.IsEmpty, m.pos, replaceWith("isEmpty", m))

      /* exists */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ HasType("Boolean"), Literal(Constant(false)))) =>
        (Elimination.Exists, m.pos, replaceWithCall("exists", m, name, body))

      /* forall */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ HasType("Boolean"), Literal(Constant(true)))) =>
        (Elimination.Forall, m.pos, replaceWithCall("forall", m, name, body))

      /* flatten */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, Ident(i) , NoneExpr())) if name == i =>
         (Elimination.Flatten, m.pos, replaceWith("flatten", m))

      /* orElse */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, SomeExpr(Ident(nameInBody)), noneBody @ (HasType("Option" | "Some" | "None")))) if name == nameInBody =>
         (Elimination.OrElse, m.pos, replaceWithExpr("orElse", m, noneBody))

      /* getOrElse */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, Ident(nameInBody), noneBody)) if name == nameInBody =>
         (Elimination.GetOrElse, m.pos, replaceWithExpr("getOrElse", m, noneBody))

      /* toList */

      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, ListExpr(Ident(name2)), NilExpr())) if name == name2 =>
         (Elimination.ToList, m.pos, replaceWith("toList", m))

    } toRight(PreparationError("No elimination candidate found."))
  }

  def perform(selection: Selection, preparationResult: PreparationResult, name: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    val (_, _, transformation) = preparationResult

    Right(transformFile(selection.file, topdown(matchingChildren(transformation))))
  }
}
