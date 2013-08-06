/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory
import analysis.TreeAnalysis
import tools.nsc.symtab.Flags

abstract class Rename extends MultiStageRefactoring with TreeAnalysis with analysis.Indexes with TreeFactory with common.InteractiveScalaCompiler  {

  import global._

  case class PreparationResult(selectedTree: SymTree, hasLocalScope: Boolean)

  type RefactoringParameters = String

  def prepare(s: Selection) = {
    s.selectedSymbolTree match {

      // Has been renamed.. also check for a matching importselector that did the rename
      case Some(t: RefTree) if t.name != t.symbol.name =>
        Right(PreparationResult(t, true))

      case Some(t) =>
        val isLocalRename = (t.symbol.isPrivate || t.symbol.isLocal) && !t.symbol.hasFlag(Flags.ACCESSOR)
        Right(PreparationResult(t, isLocalRename))
      case None => Left(PreparationError("no symbol selected found"))
    }
  }

  def perform(selection: Selection, prepared: PreparationResult, newName: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    trace("Selected tree is %s", prepared.selectedTree)

    val sym = prepared.selectedTree.symbol

    val occurences = index.occurences(sym)

    occurences foreach (s => trace("Symbol is referenced at %s (%s:%s, %s:%s)",
        s, s.pos.source.file.name, s.pos.line, s.pos.start, s.pos.end))

    val isInTheIndex = filter {
      case t: Tree => occurences contains t
    }

    val renameTree = transform {
      case t: ImportSelectorTree =>
        mkRenamedImportTree(t, newName)
      case t: SymTree =>
        mkRenamedSymTree(t, newName) setPos (t.pos withStart t.pos.start)
      case t: TypeTree =>
        mkRenamedTypeTree(t, newName, prepared.selectedTree.symbol)
      case t @ Literal(Constant(value: TypeRef)) if isClassTag(t.value) =>
        val OriginalSymbol = prepared.selectedTree.symbol
        val newType = value map {
          case TypeRef(pre, OriginalSymbol, args) =>
            // Uh..
            new Type {
              override def safeToString: String = newName
            }
          case t => t
        }
        Literal(Constant(newType)) replaces t
    }

    val rename = topdown(isInTheIndex &> renameTree |> id)

    val renamedTrees = occurences flatMap (rename(_))

    Right(refactor(renamedTrees))
  }
}
