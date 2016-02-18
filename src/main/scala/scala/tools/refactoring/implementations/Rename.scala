/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import transformation.TreeFactory
import analysis.TreeAnalysis
import tools.nsc.symtab.Flags
import scala.tools.refactoring.common.RenameSourceFileChange
import scala.tools.refactoring.common.PositionDebugging
import scala.reflect.internal.util.RangePosition
import scala.tools.refactoring.util.SourceWithMarker
import scala.tools.refactoring.util.SourceWithMarker.Movements
import scala.tools.refactoring.util.SourceWithMarker.MovementHelpers
import scala.tools.refactoring.util.SourceWithMarker.Movement
import scala.tools.refactoring.common.TextChange
import scala.tools.refactoring.common.RenameSourceFileChange
import scala.tools.refactoring.common.Change

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
    val oldName = {
      // We try to read the old name from the name position of the original symbol, since this seems to be the most
      // reliable strategy that also works well in the presence of `backtick-identifiers`.
      index.declaration(sym).flatMap { tree =>
        tree.namePosition() match {
          case rp: RangePosition => Some(rp.source.content.slice(rp.start, rp.end).mkString(""))
          case _ => None
        }
      }.getOrElse {
        trace("Cannot find old name of symbol reliably; attempting fallback")
        prepared.selectedTree.nameString
      }
    }

    def generateChanges: List[Change] = {
      val occurences = index.occurences(sym)

      occurences.foreach { s =>
        trace("Symbol is referenced at %s", PositionDebugging.formatCompact(s.pos))
        eventuallyFixModifierPositionsForLazyVals(s)
      }

      import Movements._
      val mvToSymStart = Movements.until(oldName, skipping = (comment | space | reservedName))
      val textChangesWithTrees = occurences.flatMap { occ =>
        occ.namePosition() match {
          case np: RangePosition =>
            // Unfortunately, there are cases when the name position cannot be used directly.
            // Therefore we have to use an appropriate movement to make sure we capture the correct range.
            val srcAtStart = SourceWithMarker.atStartOf(np)
            mvToSymStart(srcAtStart).map { markerAtSymStart =>
              (TextChange(np.source, markerAtSymStart, markerAtSymStart + oldName.length, newName), occ)
            }

          case _ =>
            None
        }
      }

      // Since the ASTs do not directly represent the user source code, it might be easily possible that
      // some text changes are duplicated. The code below removes them.
      val uniqTextChangesWithTrees = textChangesWithTrees.groupBy(_._1).map { case (change, changesWithTrees) =>
        (change, changesWithTrees.head._2)
      }.toList

      val textChanges = uniqTextChangesWithTrees.map(_._1)

      val newSourceChanges = uniqTextChangesWithTrees.flatMap { case (textChange, tree) =>
        if (tree.pos.source.file.name == oldName + ".scala") {
          Some(RenameSourceFileChange(tree.pos.source.file, newName + ".scala"))
        } else {
          None
        }
      }.distinct

      textChanges ::: newSourceChanges
    }

    trace(s"Old name is $oldName")
    if (oldName == newName) Right(Nil)
    else Right(generateChanges)
  }
}
