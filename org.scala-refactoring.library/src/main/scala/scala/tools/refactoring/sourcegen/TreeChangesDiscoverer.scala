/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

/**
 * Provides a function that discovers all trees that have changed
 * and need to be re-generated.
 */
trait TreeChangesDiscoverer {
  
  self: common.Tracing with common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  /**
   * Starting from a root tree, returns all children that have changed. The format
   * of the result is a list of pairs, each pair has a top-level tree and a set
   * of all trees that changed in the context of that top-level tree, including the
   * top-level tree.
   */
  def findAllChangedTrees(t: Tree): List[(Tree, Set[Tree])] = {
    
    def hasTreeInternallyChanged(t: Tree): Boolean = findOriginalTree(t) map (t → _) getOrElse { 
        trace("original not found for tree %s", t)
        return true
      } match {
        case (t: NameTree, o: NameTree) => 
          t.nameString != o.nameString
        case (t: Literal, o: Literal) =>
          t.value != o.value
        case (t: Ident, o: Ident) =>
          t.nameString != o.nameString
        case (t: TypeTree, o: TypeTree) =>
          t != o
        case _ => 
          false
      }
    
    def hasChangedChildren(t: Tree): Boolean = findOriginalTree(t) map children match {
      case None =>
        Predef.error("should never happen")
      case Some(origChld) =>
        !(children(t) corresponds origChld) { (t1, t2) =>
          if(t1.sameTree(t2))
            true
          else {
            val currentTree = t
            val originalTree = findOriginalTree(t).get
            
            val newChildren = children(t)
            val orgChildren = origChld
            trace("%s and %s are not the same trees", t1, t2)
            false
          }
        }
    }
    
    def searchChildrenForChanges(parent: Tree): List[Tree] = {
      
      def findChildren(t: Tree, parents: List[Tree]): List[Tree] = {
        if (hasTreeInternallyChanged(t)) {
          trace("  Tree %s has changed internally.", t.getClass.getSimpleName)
          t :: parents ::: searchChildrenForChanges(t)
        } else if (hasChangedChildren(t)) {
          trace("  Tree %s has changed children.", t.getClass.getSimpleName)
          t :: parents ::: searchChildrenForChanges(t)
        } else {
          //trace("  Tree %s has no changes, searching in children.", t.getClass.getSimpleName)
          children(t) flatMap (c => findChildren(c, t :: parents))
        }
      }
      
      children(parent) flatMap (findChildren(_, Nil))
    }
    
    if (hasTreeInternallyChanged(t)) {
      trace("Tree %s has changed internally.", t.getClass.getSimpleName)
      List((t, Set(t) ++ searchChildrenForChanges(t)))
    } else if (hasChangedChildren(t)) {
      trace("Tree %s has changed children.", t.getClass.getSimpleName)
      List((t, Set(t) ++ searchChildrenForChanges(t)))
    } else {
      //trace("Tree %s has no changes, searching in children.", t.getClass.getSimpleName)
      children(t) flatMap (c => findAllChangedTrees(c))
    }
  }
  
  def findTopLevelTrees(ts: List[Tree]) = {
       
    def properlyIncludes(t1: Tree, t2: Tree) = t1.pos.source == t2.pos.source && t1.pos.properlyIncludes(t2.pos)
    
    def findSuperTrees(trees: List[Tree], superTrees: List[Tree]): List[Tree] = trees match {
      case Nil => superTrees
      case t :: ts =>
      
        def mergeOverlappingTrees(ts: List[Tree]): List[Tree] = ts match {
          case Nil => t :: Nil
          case x :: xs if properlyIncludes(x, t) => x :: xs
          case x :: xs if properlyIncludes(t, x) => t :: xs
          case x :: xs => x :: mergeOverlappingTrees(xs)
        }
      
        findSuperTrees(ts, mergeOverlappingTrees(superTrees))
    }
    
    findSuperTrees(ts, Nil)
  }
}