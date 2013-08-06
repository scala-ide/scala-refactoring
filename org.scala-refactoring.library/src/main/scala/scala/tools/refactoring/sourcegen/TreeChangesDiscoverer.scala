/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import common.Tracing
import common.PimpedTrees

/**
 * Provides a function that discovers all trees that have changed
 * and need to be regenerated.
 */
trait TreeChangesDiscoverer {

  this: Tracing with PimpedTrees with common.CompilerAccess =>

  import global._

  /**
   * Starting from a root tree, returns all children that have changed. The format
   * of the result is a tuple of a top-level tree, a position of the range that should
   * be replaced and a set of all trees that changed in the context of that top-level
   * tree, including the top-level tree.
   */
  def findAllChangedTrees(t: Tree): List[(Tree, Position, Set[Tree])] = {

    /**
     * This method determines if a leaf-tree in the AST has been changed. It should not be used
     * for trees that enclose other ASTs because this would lead to unnecessarily large changes.
     */
    def hasTreeInternallyChanged(t: Tree): Boolean = {

      val originalTree = findOriginalTree(t)

      if(originalTree.isEmpty) {
        trace("original not found for tree %s", t)
        return true
      }

      val changed = (t -> originalTree.get) match {
        case (t: NameTree, o: NameTree) =>
          t.nameString != o.nameString
        case (t: NamedArgument, o: NamedArgument) =>
          t.name != o.name
        case (t: Literal, o: Literal) =>
          t.value != o.value
        case (t: Ident, o: Ident) =>
          t.nameString != o.nameString
        case (t: This, o: This) if !o.qual.isEmpty =>
          t.qual.toString != o.qual.toString
        case (t: Import, o: Import) =>
          t != o
        case _ =>
          false
      }

      changed
    }

    /**
     * Checks whether a tree has any changed children. We don't fully compare
     * the children of the original and changed trees but simply check if they
     * have corresponding types and positions.
     */
    def hasChangedChildren(newTree: Tree): Boolean = {

      def newChildrenHaveSamePosAndTypesAs(oldChildren: List[Tree]) = {
        val newChildren = children(newTree)
        (newChildren corresponds oldChildren) {
          // If we're going from the default package to an explicit package,
          // we need to re-write the parent:
          case (t1: Ident, Ident(nme.EMPTY_PACKAGE_NAME)) =>
            t1.name == nme.EMPTY_PACKAGE_NAME
          case (t1, t2) => t1.samePosAndType(t2)
        }
      }

      findOriginalTree(newTree) map (oldTree => Pair(oldTree, children(oldTree))) match {
        case None =>
          sys.error("should never happen")
        case Some(Pair(oldTree, oldChildren)) =>

          /* Comparing a flat list of children of trees won't let us catch a
           * change in the number of parameter lists in a method definition */
          val paramListsHaveSameArities = (oldTree, newTree) match {
            case (DefDef(_, _, _, args1, _, _), DefDef(_, _, _, args2, _, _)) =>
              (args1.map(_.size) corresponds args2.map(_.size)) {_ == _}
            case _ => true
          }

          !(paramListsHaveSameArities && newChildrenHaveSamePosAndTypesAs(oldChildren))
      }
    }

    def isSameAsOriginalTree(t: Tree) = {
      val originalTree = findOriginalTree(t)
      originalTree map (_ eq t) getOrElse (false)
    }

    def searchChildrenForChanges(parent: Tree): List[Tree] = {

      def findChildren(t: Tree, parents: List[Tree]): List[Tree] = {
        if (isSameAsOriginalTree(t)) {
          trace("  Tree %s is unchanged.", getSimpleClassName(t))
          Nil
        } else if (hasTreeInternallyChanged(t)) {
          trace("  Tree %s has changed internally.", getSimpleClassName(t))
          t :: parents ::: searchChildrenForChanges(t)
        } else if (hasChangedChildren(t)) {
          trace("  Tree %s has changed children.", getSimpleClassName(t))
          t :: parents ::: searchChildrenForChanges(t)
        } else {
          children(t) flatMap (c => findChildren(c, t :: parents))
        }
      }

      children(parent) flatMap (findChildren(_, Nil))
    }

    /*the default result when the tree has changed*/
    def resultWhenChanged = List((t, t.pos, Set(t) ++ searchChildrenForChanges(t)))

    if (isSameAsOriginalTree(t)) {
      trace("Top tree %s is unchanged.", getSimpleClassName(t))
      Nil
    } else if (hasTreeInternallyChanged(t)) {
      trace("Top tree %s has changed internally.", getSimpleClassName(t))
      resultWhenChanged
    } else if (hasChangedChildren(t)) {
      trace("Top tree %s has changed children.", getSimpleClassName(t))

      val isBlockOrTemplate = t match {
        case _: Block | _: Template => true
        case _ => false
      }

      lazy val originalChildren = findOriginalTree(t) map children getOrElse Nil
      lazy val modifiedChildren = children(t)

      if(isBlockOrTemplate && originalChildren.size == modifiedChildren.size) {
        val onlyDifferentChildren = originalChildren zip modifiedChildren filterNot {
          case (t1, t2) => t1.samePosAndType(t2)
        }

        // only one statement in the block has changed, so we can rewrite just this one
        // because it does not have a position, we return the position of the stmt it
        // replaced

        def replaceSingleDef(orig: Tree, changed: Tree) = {
          trace("Replace only the single changed statement in the block.")
          List((changed, orig.pos, Set(changed) ++ searchChildrenForChanges(changed)))
        }

        (t, onlyDifferentChildren) match {
          case (t: Block   , (orig, changed) :: Nil) if changed.pos == NoPosition =>
            replaceSingleDef(orig, changed)
          case (t: Template, (orig, changed) :: Nil) if changed.pos == NoPosition && t.body.contains(changed) =>
            // Only use the efficient replace when the changed tree is in the template body,
            // if it's e.g. a parent type, then we still need to print the whole template
            // because of things like the extends keyword.
            replaceSingleDef(orig, changed)
          case _ =>
            resultWhenChanged
        }
      } else {
        resultWhenChanged
      }
    } else {
      children(t) flatMap (c => findAllChangedTrees(c))
    }
  }

  def findTopLevelTrees(ts: List[Tree]) = {

    def properlyIncludes(t1: Tree, t2: Tree) = t1.pos.source == t2.pos.source && t1.pos.properlyIncludes(t2.pos)

    def findSuperTrees(trees: List[Tree], superTrees: List[Tree]): List[Tree] = trees match {
      case Nil => superTrees
      case t :: ts =>

        def mergeOverlappingTrees(t: Tree, ts: List[Tree]): List[Tree] = ts match {
          case Nil => t :: Nil
          case x :: xs if properlyIncludes(x, t) =>
            x :: mergeOverlappingTrees(x, xs)
          case x :: xs if properlyIncludes(t, x) =>
            t :: mergeOverlappingTrees(t, xs)
          case x :: xs => x :: mergeOverlappingTrees(t, xs)
        }

        findSuperTrees(ts, mergeOverlappingTrees(t, superTrees))
    }

    findSuperTrees(ts, Nil).distinct
  }
}