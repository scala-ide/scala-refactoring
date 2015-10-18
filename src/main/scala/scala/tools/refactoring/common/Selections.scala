/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import collection.mutable.ListBuffer
import tools.nsc.Global
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.Flags

trait Selections extends TreeTraverser with common.PimpedTrees {

  this: CompilerAccess =>

  import global._
  import PartialFunction._

  trait Selection {

    val pos: RangePosition

    // a tree that encloses the complete position
    val root: Tree

    def file: tools.nsc.io.AbstractFile

    /**
     * Returns all selected trees that are not
     * children of other selected trees.
     */
    lazy val selectedTopLevelTrees: List[Tree] = {
      val hits = new ListBuffer[Tree]
      // Use the global.Traverser because we don't want to
      // get inside TypeTree's original tree. The problem is
      // that if the original is an Annotated tree with a Block,
      // we might get duplicate trees. For an example, see the
      // extractFromMethodWithMultipleAssignment TestCase.
      new global.Traverser {
        override def traverse(t: Tree): Unit = {
          if (t.pos.isRange && pos.includes(t.pos)) {
            hits += t
          } else
            super.traverse(t)
        }
      }.traverse(root)
      hits.toList map skipForExpressionTrees
    }

    /**
     * Returns all symbols that are either used or
     * defined in the selected trees and their children.
     */
    lazy val selectedSymbols = allSelectedTrees flatMap {
      case t: SymTree => Some(t.symbol)
      case _ => None
    }

    /**
     * Returns the tree that encloses the whole selection.
     */
    lazy val enclosingTree = {
      var headTraversed = false
      findSelectedWithPredicate {
        case t if t == selectedTopLevelTrees.headOption.getOrElse(EmptyTree) =>
          headTraversed = true
          t.pos.includes(pos)
        case t => !headTraversed && t.pos.includes(pos)
      }.getOrElse(root)
    }

    /**
     * Returns true if the given Tree is fully contained in the selection.
     */
    def contains(t: Tree) = isPosContainedIn(t.pos, pos)

    /**
     * Returns true if the given Tree fully contains this selection.
     */
    def isContainedIn(t: Tree) = isPosContainedIn(pos, t.pos)

    /**
     * Tries to find the selected SymTree: first it is checked if the selection
     * fully contains a SymTree, if true, the first selected is returned. Otherwise
     * the result of findSelectedOfType[SymTree] is returned.
     */
    lazy val selectedSymbolTree = eventuallyFixModifierPositionsForLazyVals((root filter (cond(_) {
      case t: SymTree => contains(t)
    }) filter (t => t.pos.start < t.pos.end) match {
      case (x: SymTree) :: _ => Some(x)
      case _ => None
    }) orElse findSelectedOfType[SymTree])

    /*
     * See #1002392 if you wonder why we need this
     */
    private def eventuallyFixModifierPositionsForLazyVals(t: Option[SymTree]): Option[SymTree] = t.map {
        case dd: DefDef if dd.mods.isLazy && dd.mods.positions.isEmpty =>
          val vd = root.find {
            case vd: ValDef if vd.mods.isLazy && !vd.mods.positions.isEmpty && dd.pos.point == vd.pos.point => true
            case _ => false
          }

          vd.map { vd =>
            val nDd = dd.copy()
            nDd.mods.setPositions(vd.asInstanceOf[ValDef].mods.positions)
            nDd.copyAttrs(dd)
          }.getOrElse(dd)
        case other => other
      }

    /**
     * Finds a selected tree by its type.
     *
     * @see findSelectedWithPredicate for more information
     */
    def findSelectedOfType[T](implicit m: Manifest[T]): Option[T] =
      findSelectedWithPredicate(t => m.runtimeClass.isInstance(t)) map (_.asInstanceOf[T])

    /**
     * Finds a selected tree by a predicate. The tree does not have to be selected completely,
     * it is only checked whether this selection is contained in the tree.
     *
     * If multiple trees of the type are found, the last one (i.e. the deepest child) is returned.
     */
    def findSelectedWithPredicate(predicate: Tree => Boolean): Option[Tree] = {
      filterSelected(predicate).lastOption
    }

    def filterSelected(predicate: Tree => Boolean): List[Tree] = {
      val filterer = new FilterTreeTraverser(cond(_) {
        case t => predicate(t) && isPosContainedIn(pos, t.pos)
      })

      filterer.traverse(root)

      filterer.hits.toList
    }

    private[refactoring] lazy val allSelectedTrees: List[Tree] = {
      selectedTopLevelTrees flatMap (_ filter (t => t.pos.isRange && pos.includes(t.pos)))
    }

    private def isPosContainedIn(p1: Position, p2: Position) = {
      p1.isOpaqueRange &&
        p2.isOpaqueRange &&
        p2.includes(p1) &&
        p1.source == p2.source
    }

    /**
     * Returns a list of symbols that are used inside the selection
     * but defined outside of it.
     */
    lazy val inboundDeps: List[Symbol] = {
      val usedSymbols = selectedTopLevelTrees.flatMap { t =>
        t.collect {
          case t: RefTree if t.symbol != NoSymbol => t.symbol
        }
      }.distinct

      val definedSymbols = selectedTopLevelTrees.flatMap { t =>
        t.collect {
          case t: DefTree => t.symbol
        }
      }.distinct

      usedSymbols diff definedSymbols
    }

    /**
     * Returns only inbound dependencies that are directly or indirectly owned
     * by `owner`.
     */
    def inboundDepsOwnedBy(owner: Symbol): List[Symbol] =
      inboundDeps.filter { s =>
        s.ownerChain.exists(_.fullName == owner.fullName)
      }

    /**
     * Returns inbound dependencies that are owned by the outmost enclosing class
     * or object in the CU.
     */
    lazy val inboundLocalDeps = {
      val outmostOwnerInCU = filterSelected {
        case t: PackageDef => true
        case t: ImplDef => true
        case _ => false
      }

      if (outmostOwnerInCU.isEmpty) Nil
      else inboundDepsOwnedBy(outmostOwnerInCU.head.symbol)
    }

    /**
     * Returns a list of symbols that are defined inside the selection
     * and used outside of it.
     *
     * This implementation does not use index lookups and therefore returns
     * only outbound dependencies that are used in the same compilation unit.
     * However, this affects only class member definitions.
     *
     * It also does not cover outbound dependencies that are imported through
     * an import statement in the selected code.
     */
    lazy val outboundLocalDeps: List[Symbol] = {
      val allDefs = selectedTopLevelTrees.collect {
        case t: DefTree => t.symbol
        case Assign(t: Ident, _) => t.symbol
      }

      val nextEnclosingTree = findSelectedWithPredicate {
        case t => t.pos.includes(pos) && !t.pos.sameRange(pos)
      }.getOrElse(root)

      nextEnclosingTree.children.flatMap { child =>
        child.collect {
          case t: RefTree if !pos.includes(t.pos) && allDefs.contains(t.symbol) =>
            t.symbol
        }
      }.distinct
    }

    /**
     * All inbound dependencies that are reassigned in the selected code and used
     * afterwards.
     */
    lazy val reassignedDeps =
      inboundLocalDeps intersect outboundLocalDeps

    /**
     * Expands the selection in such a way, that partially selected
     * trees are completely selected.
     */
    def expand: Selection = {
      def posOfPartiallySelectedTrees(trees: List[Tree], newPos: Position = pos): Position = {
        trees match {
          case t :: rest if t.pos overlaps pos =>
            posOfPartiallySelectedTrees(rest, newPos union t.pos)
          case t :: rest =>
            posOfPartiallySelectedTrees(rest, newPos)
          case Nil => newPos
        }
      }

      def nearestTree = enclosingTree.children match {
        case Nil => enclosingTree
        case ts => ts.minBy(_.distanceTo(pos))
      }

      // some trees have to be selected as a whole if more than one child
      // is selected. For example if a method parameter and the body is selected,
      // we select the DefDef as a whole to get a complete selection.
      def expandToParentIfRequired(s: Selection) =
        s.enclosingTree match {
          case t @ (_: DefDef | _: Function | _: If | _: Match | _: Try | _: CaseDef) =>
            s.expandTo(t).get
          case _ => s
        }

      if (selectedTopLevelTrees.isEmpty)
        withPos(nearestTree.pos)
      else
        expandToParentIfRequired(
          expandTo(
            posOfPartiallySelectedTrees(enclosingTree.children)).get)
    }

    /**
     * Tries to expand the selection to a tree that fully contains
     * the selection but is not equal to the selection.
     */
    def expandToNextEnclosingTree: Option[Selection] =
      expandTo(findSelectedWithPredicate { t =>
        t.pos.includes(pos) && !t.samePos(pos)
      }.map(_.pos).getOrElse(NoPosition))

    /**
     * Expands the selection until `pred` evaluates to true.
     */
    def expandTo(pred: Selection => Boolean): Option[Selection] =
      if (pred(this))
        Some(this)
      else
        expandToNextEnclosingTree.flatMap(_.expandTo(pred))

    /**
     * Tries to expand the selection to `newPos`.
     */
    def expandTo(newPos: Position): Option[Selection] =
      if (newPos.isRange && newPos.includes(pos))
        Some(withPos(newPos))
      else
        None

    def withPos(newPos: Position): Selection = {
      val outer = this
      new Selection {
        val root = outer.root
        val file = outer.file
        val pos = newPos.asInstanceOf[RangePosition]
      }
    }

    /**
     * Tries to expand the selection to `tree` if the current
     * selection contains only subtrees of `tree`.
     */
    def expandTo(tree: Tree): Option[Selection] =
      expandTo(tree.pos)

    /**
     * Expands to a specific type of tree.
     */
    def expandTo[T <: Tree](implicit m: Manifest[T]): Option[Selection] =
      findSelectedOfType[T].flatMap(expandTo(_))

    /**
     * Is true if the selected code could be replaced by a value.
     *
     * E.g. the selected code represents a value although it consists
     * of more than one expression:
     * ```
     * def fn = {
     *   /*(*/val a = 2
     *   a * 100/*)*/
     * }
     * ```
     * it is replaceable by `200` without changing the methods return value.
     *
     * Note, this implementation assumes that the code has no side effects.
     */
    lazy val representsValue = {
      def isNonValuePattern(t: Tree): Boolean = t match {
        case _: Star | _: Alternative | _: UnApply | _: Bind => true
        case _: Typed => true
        case Ident(nme.WILDCARD) => true
        case Apply(_, args) => args.exists(isNonValuePattern(_))
        case _ => false
      }

      def isValue(t: Tree) = t match {
        case Ident(nme.WILDCARD) => false
        case rt: RefTree => rt.isTerm
        case tt: TermTree => !isNonValuePattern(tt)
        case _ => false
      }

      (isSingleTree || !representsArgument) &&
        (selectedTopLevelTrees match {
          case Nil => false
          case t :: Nil if t.isTerm => isValue(t)
          case ts => outboundLocalDeps.isEmpty && isValue(ts.last)
        })
    }

    lazy val isSingleTree =
      selectedTopLevelTrees.headOption.map { firstTree =>
        firstTree.pos.start == pos.start && firstTree.pos.end == pos.end
      }.getOrElse(false)

    /**
     * Is true if the selected code contains only value definitions.
     */
    lazy val representsValueDefinitions =
      !outboundLocalDeps.isEmpty &&
        !outboundLocalDeps.exists(_.isType)

    lazy val representsArgument = {
      (enclosingTree match {
        case _: Apply => true
        case _ => false
      })
    }

    lazy val representsParameter = {
      def posIn(ts: List[Tree]) =
        ts.foldLeft[Position](NoPosition) { (pos, t) =>
          t.pos union pos
        }.includes(pos)

      enclosingTree match {
        case t: ValDef => t.symbol.isValueParameter
        case _@ DefDef(_, _, _, params, _, _) =>
          posIn(params.flatten)
        case _@ Function(params, _) =>
          posIn(params)
        case _ => false
      }
    }

    /**
     * Tries to determine if the selected code contains side effects.
     *
     * Caution: `mayHaveSideEffects == false` does not guarantee that selection
     * has no side effects.
     *
     * The current implementation does check if the selection contains
     * a reference to a symbol that has a type that is somehow related to Unit.
     */
    lazy val mayHaveSideEffects = {
      allSelectedTrees.exists(cond(_) {
        case t: RefTree => t.symbol.tpe.exists(_.toString == "Unit")
      })
    }

    override def toString = {
      s"Selection(${PositionDebugging.format(pos)})"
    }
  }

  def skipForExpressionTrees(t: Tree) = t match {
    case t @ TypeApply(fun: Select, args) if fun.pos.eq(t.pos) && fun.pos.eq(fun.qualifier.pos) =>
      fun.qualifier
    case t @ Select(qualifier, nme) if t.pos.eq(qualifier.pos) && nme.toTermName.toString == "withFilter" =>
      qualifier
    case t => t
  }

  case class FileSelection(file: tools.nsc.io.AbstractFile, root: Tree, from: Int, to: Int) extends Selection {

    @deprecated("Please use the primary constructor.", "0.4.0")
    def this(file: tools.nsc.io.AbstractFile, from: Int, to: Int) = {
      this(file, compilationUnitOfFile(file).get.body, from, to)
    }

    lazy val pos = new RangePosition(root.pos.source, from, from, to)
  }

  object FileSelection {
    @deprecated("Please use the primary constructor.", "0.4.0")
    def apply(file: tools.nsc.io.AbstractFile, from: Int, to: Int) = new FileSelection(file: tools.nsc.io.AbstractFile, from: Int, to: Int)
  }

  case class TreeSelection(root: Tree) extends Selection {

    if (!root.pos.isRange)
      error("Position not a range.")

    val pos = root.pos.asInstanceOf[RangePosition]

    val file = pos.source.file
  }
}
