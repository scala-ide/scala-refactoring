/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory
import analysis.TreeAnalysis
import tools.nsc.symtab.Flags
import scala.tools.refactoring.common.RenameSourceFileChange
import scala.tools.refactoring.common.PositionDebugging

abstract class Rename extends MultiStageRefactoring with TreeAnalysis with analysis.Indexes with TreeFactory with common.InteractiveScalaCompiler {

  import global._

  case class PreparationResult(selectedTree: SymTree, hasLocalScope: Boolean)

  type RefactoringParameters = String

  def prepare(s: Selection) = {

    def isLocalRename(t: Tree) = {
      def isHiddenOrNoAccessor(s: Symbol) = {
        s == NoSymbol || s.isPrivate
      }

      def hasHiddenOrNoAccessor = {
        if (t.symbol.isVal || t.symbol.isVar) {
          def getter = t.symbol.getter(t.symbol.owner)
          def setter = t.symbol.setter(t.symbol.owner)
          isHiddenOrNoAccessor(getter) && isHiddenOrNoAccessor(setter)
        } else {
          true
        }
      }

      t.symbol.isLocal || (t.symbol.isPrivate && hasHiddenOrNoAccessor)
    }

    s.selectedSymbolTree match {

      // Has been renamed.. also check for a matching importselector that did the rename
      case Some(t: RefTree) if t.name != t.symbol.name =>
        Right(PreparationResult(t, true))

      case Some(t) =>
        Right(PreparationResult(t, isLocalRename(t)))
      case None => Left(PreparationError("no symbol selected found"))
    }
  }

  def perform(selection: Selection, prepared: PreparationResult, newName: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    /*
     * A lazy val is represented by a ValDef and an associated DefDef that contains the initialization code.
     * Unfortunately, the Scala compiler does not set modifier positions for the DefDef, which is a problem for us,
     * because we are relying on the DefDef when printing out source code. The following function patches the affected
     * DefDefs accordingly.
     *
     * See #1002392 and #1002502
     */
    def eventuallyFixModifierPositionsForLazyVals(t: Tree): Unit = t match {
      case dd: DefDef if dd.mods.isLazy && dd.mods.positions.isEmpty =>
        val vd = selection.root.find {
          case vd: ValDef if vd.mods.isLazy && !vd.mods.positions.isEmpty && dd.pos.point == vd.pos.point => true
          case _ => false
        }

        vd.foreach { vd =>
          // Note that we patch the DefDef in place because we need the correctly set positions in the tree
          // later in the refactoring process. A precursor of this code pretended to be pure, by copying
          // dd and then calling 'mods.setPositions' on the copy. This had exactly the same (needed) side effects
          // however, as the Modifier object affected by setPositions was the same anyway. If you are interested
          // in the related discussion, take a look at https://github.com/scala-ide/scala-refactoring/pull/78.
          dd.mods.setPositions(vd.asInstanceOf[ValDef].mods.positions)
        }

      case _ => ()
    }

    trace("Selected tree is %s", prepared.selectedTree)

    val sym = prepared.selectedTree.symbol

    val occurences = index.occurences(sym)

    occurences.foreach { s =>
      trace("Symbol is referenced at %s", PositionDebugging.formatCompact(s.pos))
      eventuallyFixModifierPositionsForLazyVals(s)
    }

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

    val renamedTreesWithOriginals = occurences.flatMap { tree =>
      rename(tree).map((_, tree))
    }

    val renameSourceChanges = renamedTreesWithOriginals.collect {
      case (newTree: ImplDef, oldTree: ImplDef) if sourceShouldBeRenamed(newTree, oldTree) =>
        RenameSourceFileChange(oldTree.pos.source.file, newTree.name.toString() + ".scala")
    }.distinct

    Right(refactor(renamedTreesWithOriginals.map(_._1)) ++ renameSourceChanges)
  }

  private def sourceShouldBeRenamed(newTree: ImplDef, oldTree: ImplDef) = {
    lazy val namesDefined = newTree.name != null && oldTree.name != null
    lazy val namesDifferent = newTree.name != oldTree.name
    lazy val fileNamedLikeOldTree = oldTree.pos.isDefined && oldTree.pos.source.file.name == oldTree.name.toString + ".scala"

    namesDefined && namesDifferent & fileNamedLikeOldTree
  }
}
