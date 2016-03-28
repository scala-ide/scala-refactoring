/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import transformation.TreeFactory
import analysis.TreeAnalysis
import scala.tools.refactoring.common.RenameSourceFileChange
import scala.tools.refactoring.common.PositionDebugging
import scala.reflect.internal.util.RangePosition
import scala.tools.refactoring.util.SourceWithMarker
import scala.tools.refactoring.util.SourceWithMarker.Movements
import scala.tools.refactoring.common.TextChange
import scala.tools.refactoring.common.RenameSourceFileChange
import scala.tools.refactoring.common.Change
import scala.reflect.internal.util.OffsetPosition

abstract class Rename extends MultiStageRefactoring with TreeAnalysis with analysis.Indexes with TreeFactory with common.InteractiveScalaCompiler {

  import global._

  case class PreparationResult(selectedTree: SymTree, hasLocalScope: Boolean)

  type RefactoringParameters = String

  def prepare(s: Selection) = {

    /*
     * This heuristic decides if this refactoring is local to a single file or not. Note that
     * we could simply query the index for all occurrences of the symbol in question, to see if
     * multiple files are involved or not, but this would require clients to make sure that
     * we always get a fully initialized index.
     */
    def isLocalRename(t: Tree) = {
      def isLocalSymbol(symbol: Symbol) = {
        def isHiddenOrNoAccessor(symbol: Symbol) = symbol == NoSymbol || symbol.isPrivate

        def hasHiddenOrNoAccessor = {
          if (symbol.isVal || symbol.isVar) {
            def getter = symbol.getter(symbol.owner)
            def setter = symbol.setter(symbol.owner)


            isHiddenOrNoAccessor(getter) && isHiddenOrNoAccessor(setter)
          } else {
            true
          }
        }

        val relatedCtor = s.root.find {
          case dd: DefDef if dd.symbol.isConstructor && !dd.mods.isPrivate && !dd.mods.isPrivateLocal =>
            val relatedParam = dd.vparamss.flatten.find { p =>
              val (p1, p2) = (p.symbol.pos, symbol.pos)
              p1.isDefined && p2.isDefined && p1.point == p2.point
            }

            relatedParam.nonEmpty

          case _ => false
        }

        val isCtorArg = relatedCtor.nonEmpty

        if (isCtorArg) {
          // Better be safe than sorry and assume that constructor arguments might always leak out:
          //  Deciding if constructor arguments might 'leak' out of a file is a non-trivial endeavor,
          //  even if we the know that the class definition is nested in a local block. To do this
          //  properly we would have to examine all supertypes, as well as the return type of the
          //  block, which might be a structural type.
          false
        } else if (symbol.isParameter) {
          val isParamThatMightBeVisibleInOtherFiles = {
            val isNestedInMethodValOrVar = {
              def isMethodValOrVar(s: Symbol) = {
                s.isVal || s.isVar || s.isMethod
              }

              val level = symbol.ownerChain.count(isMethodValOrVar)
              level > 2
            }

            !isNestedInMethodValOrVar
          }

          !isParamThatMightBeVisibleInOtherFiles
        } else {
          symbol.isLocal || (symbol.isPrivate && hasHiddenOrNoAccessor)
        }
      }

      t.symbol match {
        case null | NoSymbol => true
        case properSymbol => isLocalSymbol(properSymbol)
      }
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
      val mvToSymStartForRangePos = Movements.until(oldName, skipping = (comment | space | reservedName))
      val textChangesWithTrees = occurences.flatMap { occ =>
        occ.namePosition() match {
          case np: RangePosition =>
            // Unfortunately, there are cases when the name position cannot be used directly.
            // Therefore we have to use an appropriate movement to make sure we capture the correct range.
            val srcAtStart = SourceWithMarker.atStartOf(np)
            mvToSymStartForRangePos(srcAtStart).map { markerAtSymStart =>
              (TextChange(np.source, markerAtSymStart, markerAtSymStart + oldName.length, newName), occ)
            }

          case op: OffsetPosition =>
            // Normally, we would not like to deal with offset positions here at all.
            // Unfortunately the compiler emits them instead of range positions in
            // interpolated strings like f"$x" (see #1002651).
            val pointIsAtStartOfNameToBeChanged = {
              val srcBeforePoint = SourceWithMarker.atPoint(op).step(forward = false)
              val pointIsInMiddleOfId = Movements.id(srcBeforePoint).exists(_ > op.point)

              if (pointIsInMiddleOfId) {
                false
              } else {
                val srcAtPoint = srcBeforePoint.step(forward = true)
                val srcAfterId = srcAtPoint.moveMarker(Movements.id)
                val consumedId = srcAtPoint.source.slice(srcAtPoint.marker, srcAfterId.marker).mkString("")
                consumedId == oldName
              }
            }

            if (pointIsAtStartOfNameToBeChanged) {
              Some((TextChange(op.source, op.point, op.point + oldName.length, newName), occ))
            } else {
              None
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
