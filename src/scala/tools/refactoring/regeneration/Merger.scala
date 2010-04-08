/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.regeneration

import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.common.Tracing
import PartialFunction._

trait Merger {
  self: LayoutHandler with Tracing with SourceHelper with Fragments with FragmentRepository =>
    
  def merge(rootScope: TreeScope, allFragments: FragmentRepository, hasTreeChanged: global.Tree => Boolean) = context("merge fragments") {
    
    def getLayoutBetween(current: Fragment, next: Fragment, scope: Scope) = {
      
      def extractOriginalLayout = (current, next) match {
        case (c: OriginalSourceFragment, n: OriginalSourceFragment) if n.isEndOfScope && n.start > rootScope.end => 
          /* because we replace just parts of the original source file,
           * it can happen that we take too much of the layout, duplicating
           * existing layout when applying the patch.
           * */
          val l = c layout n
          val overflow = n.start - rootScope.end
          l.substring(0, l.length - overflow)
        case (c: OriginalSourceFragment, n: OriginalSourceFragment) => c layout n
      }
      
      def getLayoutAfterCurrentFragment = if(allFragments exists current)
        splitLayoutBetween(allFragments getNext current)._1
      else ""
        
      def getLayoutBeforeOriginalNextFragment = if(allFragments exists next)
        splitLayoutBetween(allFragments getPrevious next)._2
      else ""
      
      val layoutWithRequisites = if (keepOldLayout(current, next, scope)) {
        extractOriginalLayout \\ (trace("%s and %s are in the original order and enclose layout %s", current, next, _))
      } else {
        trace("current: %s and next: %s have been rearranged", current, next)

        val layoutAfterCurrent = getLayoutAfterCurrentFragment

        val layoutBeforeNext = getLayoutBeforeOriginalNextFragment
        
        processRequisites(current, layoutAfterCurrent, layoutBeforeNext, next) \\ (trace("layout is %s", _))
      }
	    
	    val existingIndentation = allFragments.scopeIndentation(next) flatMap (s => indentationLength(next) map (s → _))
	    
	    fixIndentation(layoutWithRequisites, existingIndentation, next.isEndOfScope, scope.indentation) \\ (trace("the resulting layout is %s", _))
    }
    
    def keepOldLayout(current: Fragment, next: Fragment, scope: Scope) = {
    
      val currentExists = allFragments exists current
      
      val nextIsOriginal = cond(allFragments getNext current) { case Some((_, originalNext)) => originalNext == next }
      
      def orderHasNotChanged = currentExists && nextIsOriginal
      
      def isNotCompilationUnitRoot = scope == rootScope && rootScope != allFragments.root
      
      /* 
       * if the root scope of the modification isn't the root of the compilation unit, then we have to ignore the changes
       * between the begin of the scope and the first element, and the same for the end of the scope, because the scope
       * nodes then cannot be found in the original source and this would lead to unnecessary re-generation of code.
       * */
      def isBeginOrEnd = (current.isBeginOfScope || (currentExists && next.isEndOfScope)) && isNotCompilationUnitRoot
      
      orderHasNotChanged || isBeginOrEnd
    }
    
    def justMiddleFragmentReplaced(x: Fragment, y: Fragment, z: Fragment) = !y.isInstanceOf[Scope] && (allFragments.exists(x) || x.isBeginOfScope) && !allFragments.exists(y) && allFragments.exists(z)
        
    def traverseScopeAndMergeChildrenWithLayout(scope: Scope) = {
      def recurse(l: List[Fragment]): List[Fragment] = l match {
        
        case (x: Scope) :: y :: rest => innerMerge(x) ::: new StringFragment(getLayoutBetween(x, y, scope)) :: recurse(y :: rest)
         
        case (x: OriginalSourceFragment) :: (y: OriginalSourceFragment) :: (z: OriginalSourceFragment) :: rest if justMiddleFragmentReplaced(x, y, z) =>

            trace("the middle fragment %s between %s and %s has been changed", y, x, z)
        
            def originalLayout(pair: Option[(OriginalSourceFragment, OriginalSourceFragment)], l: Fragment, r: Fragment) =
              pair map Function.tupled(_ layout _) map (processRequisites(l, _, "", r)) map (new StringFragment(_)) getOrElse(new StringFragment(""))

            def getTrailingOriginalLayout(f: Fragment) = originalLayout(allFragments getNext f, f, y)
            
            def getLeadingOriginalLayout(f: Fragment) = originalLayout(allFragments getPrevious f, y, f)
            
            def keepAllOriginalLayout = getTrailingOriginalLayout(x) :: y :: getLeadingOriginalLayout(z) :: Nil
            
            x :: keepAllOriginalLayout ::: recurse(z :: rest)
            
        case  x :: y :: rest => x :: new StringFragment(getLayoutBetween(x, y, scope)) :: recurse(y :: rest)
            
        case _ => Nil
      }
      recurse(scope.children)
    }
    
    def layoutChanges(current: Fragment, next: Fragment, scope: Scope) = {
      !keepOldLayout(current, next, scope) || allFragments.scopeIndentation(next).getOrElse(-1) != scope.indentation 
    }
    
    def hasScopeChanges(scope: Scope): Boolean = scope.children.iterator.sliding(2) exists {
      case (s: Scope) :: next :: _ if hasScopeChanges(s) => true
      case current    :: next :: _                       => layoutChanges(current, next, scope)
      case _ => false
    }
              
    def scopeHasArtificialTree(s: Scope) = s.exists(_.isInstanceOf[ArtificialTreeFragment])
      
    def scopeHasNoChanges(s: Scope with WithTree) = !(hasTreeChanged(s.tree) || scopeHasArtificialTree(s)) || !hasScopeChanges(s)

    def unchangedScope(scope: OriginalSourceFragment) = new Fragment {
      def print = scope.file.content.slice(scope.start, scope.end)
    } :: Nil

    def innerMerge(scope: Scope): List[Fragment] = context("inner merge loop") {

      scope match {
        
        case scope: OriginalSourceFragment with WithTree if scopeHasNoChanges(scope) => 
          trace("scope %s has not changed", scope)
          unchangedScope(scope)
          
        case _ =>
          trace("scope %s has changed, because it "+ (scope match {
            case s: OriginalSourceFragment with WithTree if hasTreeChanged(s.tree) =>
              "is in changeset"
            case s: OriginalSourceFragment with WithTree if scopeHasArtificialTree(s) =>
              "contains artificial trees"
            case s: OriginalSourceFragment =>
              "does not contain a tree"
            case _ => 
              "is not an OriginalSourceFragment"
          }), scope)
          
          traverseScopeAndMergeChildrenWithLayout(scope)
      }
    }
    
    innerMerge(rootScope) map (_.render(allFragments) mkString)
  }
}
