package scala.tools.refactoring
package sourcegen

/**
 * Provides a function that discovers all trees that have changed
 * and need to be re-generated.
 * 
 * Describe the basic algorithm here.
 * */
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
    
    def hasTreeInternallyChanged(t: Tree): Boolean = findOriginalTree(t) map (t → _) getOrElse(return true) match {
      case (t: NameTree, o: NameTree) => 
        t.nameString != o.nameString
      case (t: Literal, o: Literal) =>
        t.value != o.value
      case _ => 
        false
    }
    
    def hasChangedChildren(t: Tree): Boolean = findOriginalTree(t) map children match {
      case None =>
        Predef.error("should never happen")
      case Some(origChld) =>
        !(children(t) corresponds origChld)(_ sameTree _)
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
          trace("  Tree %s has no changes, searching in children.", t.getClass.getSimpleName)
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
      trace("Tree %s has no changes, searching in children.", t.getClass.getSimpleName)
      children(t) flatMap (c => findAllChangedTrees(c))
    }
  }
}