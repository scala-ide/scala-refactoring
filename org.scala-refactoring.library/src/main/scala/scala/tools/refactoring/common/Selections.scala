/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import collection.mutable.ListBuffer
import tools.nsc.Global
import scala.reflect.internal.util.RangePosition

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
        override def traverse(t: Tree) {
          if (t.pos.isRange && pos.includes(t.pos)) {
            hits += t
          } else
            super.traverse(t)
        }
      }.traverse(root)
      hits.toList
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
    lazy val selectedSymbolTree = (root filter (cond(_) {
      case t: SymTree => contains(t)
    }) filter (t => t.pos.start < t.pos.end) match {
      case (x: SymTree) :: _ => Some(x)
      case _ => None
    }) orElse findSelectedOfType[SymTree]

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

      val filterer = new FilterTreeTraverser(cond(_) {
        case t => predicate(t) && isPosContainedIn(pos, t.pos)
      })

      filterer.traverse(root)

      filterer.hits.lastOption
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

    if(!root.pos.isRange)
      error("Position not a range.")

    val pos = root.pos.asInstanceOf[RangePosition]

    val file = pos.source.file
  }
}
