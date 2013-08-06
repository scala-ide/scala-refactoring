/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import tools.nsc.symtab.Flags
import transformation.TreeFactory
import scala.tools.refactoring.analysis.GlobalIndexes

abstract class ExpandCaseClassBinding extends MultiStageRefactoring with GlobalIndexes {

  this: common.CompilerAccess =>

  import global._

  case class PreparationResult(bind: Bind, sym: ClassSymbol, body: Tree)

  class RefactoringParameters

  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {

    def failure = Left(PreparationError("No binding to expand found. Please select a binding in a case clause."))

    val res = s.findSelectedOfType[CaseDef] flatMap { caseDef =>
      s.findSelectedOfType[Bind] map {
        case bind @ Bind(_, body) =>
          body.tpe match {
            case TypeRef(_, sym: ClassSymbol, _) if sym.isCaseClass =>
              Right(PreparationResult(bind, sym, caseDef.body))
            case _ => failure
          }
      }
    }

    res getOrElse failure
  }

  def perform(selection: Selection, preparationResult: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    val PreparationResult(bind, sym, body) = preparationResult

    val apply = {

      val argSymbols = sym.info.decls.toList filter (s => s.isCaseAccessor && s.isMethod)

      val argIdents = argSymbols map (s => Ident(s.name))
      val isTuple = sym.nameString.matches("Tuple\\d+")

      if(isTuple) {
        // don't insert TupleX in the code
        Apply(Ident(""), argIdents)
      } else {
        Apply(Ident(sym.name), argIdents)
      }
    }

    val replacement = {
      // we create a mini-index of the casedef-body
      val index = GlobalIndex(body)
      val nameIsNotReferenced = index.references(bind.symbol).isEmpty

      if(nameIsNotReferenced) {
        apply
      } else {
        // creates a `name @ ` in the code
        bind copy (body = apply)
      }
    } replaces bind

    Right(refactor(List(replacement)))
  }

  def index: IndexLookup = sys.error("")
}
