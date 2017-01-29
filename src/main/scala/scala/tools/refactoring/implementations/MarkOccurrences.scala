/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import scala.reflect.internal.util.OffsetPosition
import scala.reflect.internal.util.RangePosition
import scala.tools.refactoring.common.PositionDebugging
import scala.tools.refactoring.util.SourceWithMarker
import scala.tools.refactoring.util.SourceWithMarker.Movements
import scala.util.control.NonFatal
import scala.tools.refactoring.util.SourceWithMarker.Movement
import scala.tools.refactoring.common.TracingHelpers
import scala.tools.refactoring.util.SourceHelpers
import scala.tools.refactoring.util.SourceWithSelection

import scala.PartialFunction.condOpt

trait MarkOccurrences extends common.Selections with analysis.Indexes with common.CompilerAccess with common.EnrichedTrees with common.InteractiveScalaCompiler {
  import global._
  import TracingHelpers.toCompactString

  private def tryFindMissingSymbol(treeWithoutSymbol: Tree, root: Tree): Symbol = {
    treeWithoutSymbol match {
      case treeWithoutSymbol: RefTree =>
        tryFindMissingSymbol(treeWithoutSymbol: RefTree, root: Tree)

      case _ =>
        NoSymbol
    }
  }

  /*
   * In some cases (for example when dealing with refined self types), the selected tree might not
   * have a symbol. This method attempts to find it anyway.
   */
  private def tryFindMissingSymbol(treeWithoutSymbol: RefTree, root: Tree): Symbol = {
    root.foreach { t =>
      condOpt(t) { case t: TypeTree =>
        if (t.original == treeWithoutSymbol) {
          t.tpe.parents.foreach { p =>
            condOpt(p) {
              case p: TypeRef if p.sym.name == treeWithoutSymbol.name =>
                  return p.sym
            }
          }
        } else {
          condOpt(t.original) {
            case orig: RefTree if orig.qualifier == treeWithoutSymbol =>
              t.tpe.parents.foreach { p =>
                condOpt(p) { case p: TypeRef =>
                  condOpt(p.pre) {
                    case pre: ThisType if pre.sym.name.toString == treeWithoutSymbol.name.toString =>
                      return pre.sym
                  }
                }
              }

            case orig: CompoundTypeTree =>
              val tParents = t.tpe.parents.flatMap {
                case t: RefinedType => t.parents
                case _ => Nil
              }

              val oParents = orig.templ.parents

              if (oParents.size == tParents.size) {
                oParents.zip(tParents).foreach { case (oTree, tRef) =>
                  condOpt(oTree) {
                    case refTree: RefTree =>
                      val sym = findSymbolInType(treeWithoutSymbol, refTree)(tRef)
                      if (sym != NoSymbol) {
                        return sym
                      }
                  }
                }
              }
          }
        }
      }
    }

    NoSymbol
  }

  private def findSymbolInType(treeWithoutSymbol: Tree, ref: RefTree)(tpe: Type): Symbol = {
    condOpt(tpe) {
      case tRef: TypeRef if ref.name == tRef.sym.name =>
        val res = condOpt(tRef.pre) {
          case pre: ThisType if ref.qualifier.pos.isRange && treeWithoutSymbol == ref.qualifier =>
            pre.sym
        }

        res.getOrElse {
          if (ref == treeWithoutSymbol && ref.namePosition().isRange) tRef.sym
          else NoSymbol
        }
    }.getOrElse {
      NoSymbol
    }
  }

  protected class SingleTreeSelection(val selected: Tree, val root: Tree) {
    val symbol = selected match {
      case Import(expr, List(selector)) =>
        findSymbolForImportSelector(expr, selector.name).getOrElse(NoSymbol)

      case tree if tree.symbol != NoSymbol =>
        tree.symbol

      case treeWithoutSymbol =>
        trace("Selected tree does not have symbol")
        tryFindMissingSymbol(treeWithoutSymbol, root)
    }

    val name = {
      val declaration = index.declaration(symbol)
      trace(s"Selected symbol $symbol is decared at ${toCompactString(declaration)}")

      declaration.flatMap { declaration =>
        // We try to read the old name from the name position of the declaration, since this seems to be the most
        // reliable strategy that also works well in the presence of `backtick-identifiers`.
        declaration.namePosition() match {
          case rp: RangePosition =>
            if (SourceWithMarker.atStartOf(rp).moveMarker(Movements.id).marker != rp.end) {
              trace(s"Name position at ${PositionDebugging.format(rp)} seems to be invalid")
              None
            } else {
              Some(rp.source.content.slice(rp.start, rp.end).mkString(""))
            }
          case op =>
            trace(s"Expected range position, but found $op")
            None
        }
      }.getOrElse {
        try {
          selected match {
            case Import(_, List(selector)) => selector.rename.toString
            case _ => selected match {
              case s: Select => s.name.decoded
              case other => other.nameString
            }
          }
        } catch {
          case NonFatal(e) =>
            trace("Cannot find old name of symbol; giving up")
            "<no-name>"
        }
      }
    } \\ { name =>
      trace(s"SingleTreeSelection.name: $name")
    }
  }

  /*
   * Self references aka `class Foo { self =>` need special treatment, because their representation in typed
   * ASTs is incomplete. While `self =>` is represented as a synthetic `ValDef`, usages of the named reference
   * can not be distinguished from the `this` keyword, without looking into the source code. This method
   * takes care of this special case.
   */
  private def findOccurrencesForSelfReference(selection: SingleTreeSelection): Option[List[(RangePosition, Tree)]] = {
    def root = selection.root
    def selected = selection.selected
    val parent = findParentOfPotentialSelfReference(selected, root)

    parent.map { parent =>
      val owningClassSymbol = {
        parent.symbol.ownersIterator.find(_.isClass).getOrElse {
          throw new IllegalStateException(s"Could not find enclosing class symbol for $parent")
        }
      }

      def isSelfReference(tis: This): Boolean = {
        tis.symbol == owningClassSymbol && SourceHelpers.stringCoveredBy(tis.pos).exists(_ != "this")
      }

      val referencesViaThis = parent.collect {
        case tis: This if isSelfReference(tis) => (tis.pos.asInstanceOf[RangePosition], tis)
      }

      (selected.pos.asInstanceOf[RangePosition], selected) :: referencesViaThis
    }
  }

  protected final def findParentOfPotentialSelfReference(potentialRef: Tree, root: Tree): Option[Template] = potentialRef match {
    case vd: ValDef if vd.rhs.isEmpty =>
      val parentTemplate = {
        root.find {
          case tmpl: Template if tmpl.self == vd => true
          case _ => false
        }
      }

      parentTemplate.map(_.asInstanceOf[Template])

    case _ =>
      None
  }

  protected def findOccurrences(selection: SingleTreeSelection): List[(RangePosition, Tree)] = {
    findOccurrencesForSelfReference(selection)
      .getOrElse(findOccurrencesUsingIndex(selection))
  }

  private def findOccurrencesUsingIndex(selection: SingleTreeSelection): List[(RangePosition, Tree)] = {
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

    /*
     * ImportSelectorTrees need special treatment, since it is not a priory clear which side (the name or rename side) of
     * the tree is relevant for us.
     */
    def positionsWithPotentiallyReferencingTrees(trees: Seq[Tree]): Seq[(Position, Tree)] = {
      trees.flatMap {
        case ImportSelectorTree(name, rename) => Seq(name.namePosition() -> name, rename.namePosition() -> rename)
        case other => Seq(other.namePosition() -> other)
      }
    }

    val referencingTrees = index.occurences(selection.symbol)

    referencingTrees.foreach { s =>
      trace("Symbol is referenced at %s", PositionDebugging.formatCompact(s.pos))
      eventuallyFixModifierPositionsForLazyVals(s)
    }

    import Movements._
    val mvToSymStartForRangePos = Movements.until(selection.name, skipping = (comment | space | reservedName))
    val occurences = positionsWithPotentiallyReferencingTrees(referencingTrees).flatMap { case (pos, occ) =>
      pos match {
        case np: RangePosition =>
          // Unfortunately, there are cases when the name position cannot be used directly.
          // Therefore we have to use appropriate movements to make sure we capture the correct range.
          val srcAtStart = SourceWithMarker.atStartOf(np)
          mvToSymStartForRangePos(srcAtStart).flatMap { markerAtSymStart =>
            val consumedId = Movement.coveredString(markerAtSymStart, srcAtStart.source, Movements.id)

            if (consumedId == selection.name) {
              Some((new RangePosition(np.source, markerAtSymStart, markerAtSymStart, markerAtSymStart + selection.name.length), occ))
            } else {
              trace(s"consumedId `$consumedId` does not match selecton name `${selection.name}`")
              None
            }
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
              val consumedId = Movement.coveredString(srcAtPoint, Movements.id)
              consumedId == selection.name
            }
          }

          if (pointIsAtStartOfNameToBeChanged) {
            Some((new RangePosition(op.source, op.point, op.point, op.point + selection.name.length), occ))
          } else {
            None
          }

        case _ =>
          None
      }
    }

    // Since the ASTs do not directly represent the user source code, it might be easily possible that
    // some ranges are duplicated. The code below removes them.
    val uniqOccurences = occurences.groupBy(_._1).map { case (pos, occWithTrees) =>
      (pos, occWithTrees.head._2)
    }.toList

    uniqOccurences
  }

  private def traceSelection(selection: FileSelection, selectedTree: Option[Tree], from: Int, to: Int): Unit = {
    def rawSrc = selection.root.pos.source.content
    def srcFrom = SourceWithMarker(rawSrc, from)
    def srcTo = SourceWithMarker(rawSrc, to)

    def selectedTreeName = {
      selectedTree.map { selectedTree =>
        try {
          selectedTree.nameString
        } catch {
          case NonFatal(e) =>
            selectedTree.toString
        }
      }
    }

    trace(s"from        : $srcFrom")
    trace(s"to          : $srcTo")
    trace(s"selectedTree: $selectedTreeName")
  }

  private def eventuallyTraceAndReturnOccurences(treeWithOccurences: (Tree, List[Position])): (Tree, List[Position]) = {
    trace {
      val formattedPositions = {
        val occurences = treeWithOccurences._2

        if (occurences.nonEmpty) occurences.map(PositionDebugging.format).mkString(", ")
        else "<none>"
      }

      s"Returning occurrences for ${toCompactString(treeWithOccurences._1)}: $formattedPositions"
    }

    treeWithOccurences
  }

  def occurrencesOf(file: tools.nsc.io.AbstractFile, from: Int, to: Int): (Tree, List[Position]) = context("occurrencesOf") {


    val treeWithOccurences = {
      val selection = new FileSelection(file, global.unitOfFile(file).body, from, to)
      val selectedTree = selection.selectedSymbolTree
      traceSelection(selection, selectedTree, from, to)

      val selectionOnScalaId = {
        val positionsToCheck = selectedTree.toSeq.flatMap {
          case imp: Import =>
            // Imports need special handling, since it is not clear on which part of the import statement the cursor might be positioned on
            Seq(imp.expr.namePosition()) ++ imp.selectors.map(s => new OffsetPosition(imp.pos.source, s.namePos))

          case other => Seq(other.namePosition())
        }

        def isPlausible(pos: Position): Boolean = {
          val src = pos match {
            case rp: RangePosition => Some(SourceWithMarker.atStartOf(rp))
            case op: OffsetPosition => Some(SourceWithMarker.atPoint(op))
            case _ => None
          }

          src.exists { src =>
            val selectedId = Movement.coveredString(src, Movements.id)
            if (selectedId == "") {
              false
            } else {
              if (src.marker <= from && src.marker + selectedId.length >= to) {
                true
              } else {
                SourceHelpers.isRangeWithin(selectedId, SourceWithSelection(src.source, from, to))
              }
            }
          }
        }

        positionsToCheck.exists(isPlausible)
      } \\ { selectionOnScalaId =>
        trace(s"selectionOnScalaId: $selectionOnScalaId")
      }

      if (!selectionOnScalaId) {
        (EmptyTree, Nil)
      } else {
        val occurrences = selectedTree.toList.flatMap { selectedTree =>
          val singleTreeSelection = new SingleTreeSelection(selectedTree, selection.root)
          val occurences = findOccurrences(singleTreeSelection)
          occurences.map(_._1)
        }

        (selectedTree.getOrElse(EmptyTree), occurrences)
      }
    }

    eventuallyTraceAndReturnOccurences(treeWithOccurences)
  }
}
