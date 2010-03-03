package scala.tools.refactoring.regeneration

import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.util.Tracing
import PartialFunction._

trait Merger {
  self: LayoutHandler with Tracing with SourceHelper with Fragments with FragmentRepository =>
  
  def merge(rootScope: Scope, allFragments: FragmentRepository, hasTreeChanged: global.Tree => Boolean): List[Fragment] = context("merge fragments") {
    
    def getLayoutBetween(current: Fragment, next: Fragment, scope: Scope): (Fragment, Boolean) = {
      
      val keepOldLayout = {
        
        val currentExists = allFragments exists current
        
        def orderHasNotChanged = currentExists && cond(allFragments getNext current) { case Some((_, _2)) => _2 == next }
        
        def isNotCompilationUnitRoot = scope == rootScope && rootScope != allFragments.root
        
        /* 
         * if the root scope of the modification isn't the root of the compilation unit, then we have to ignore the changes
         * between the begin of the scope and the first element, and the same for the end of the scope, because the scope
         * nodes then cannot be found in the original source and this would lead to unnecessary re-generation of code.
         * */
        def isBeginOrEnd = (current.isBeginOfScope || (currentExists && next.isEndOfScope)) && isNotCompilationUnitRoot
        
        orderHasNotChanged || isBeginOrEnd
      }
          
      val (layout, somethingChanged) = if (keepOldLayout) {
        val layout = (current, next)  match {
          case (c: OriginalSourceFragment, n: OriginalSourceFragment) => c layout n
        }
        trace("%s and %s are in the original order and enclose %s", current, next, layout)
        (layout, false)
      } else {
        trace("%s and %s have been rearranged", current, next)
        /*
         * The tree has been re-arranged, the next part in the original source isn't our current next. 
         * We have to split the layout between our current part and its next part in the original 
         * source
         * */
        val (layoutAfterCurrent, _) =
          if(allFragments exists current) {
            splitLayoutBetween(allFragments getNext current)
          } else ("", "")
        
        /*
         * We also need the layout of our right neighbor:
         * */
        val (_, layoutBeforeNext) =
          if(allFragments exists next) {
            splitLayoutBetween(allFragments getPrevious next)
          } else ("", "")
        
        /*
         * We now have 4 fragments of layout, we only need the left slice of our current fragment (that is, 
         * the layout that is adjacent to the current fragment) and the (right) slice of layout that is directly
         * before the next fragment in the tree. Combined, we get all layout we need.
         * */
        
        trace("the next fragment is %s", next)
        
        val layout = processRequisites(current, layoutAfterCurrent, layoutBeforeNext, next)
        
        trace("layout is %s", layout)
        
        (layout, true)
      }
	    
	    val existingIndentation = allFragments.scopeIndentation(next) flatMap (s => indentationLength(next) map (s → _))
	    
	    val indentedLayout = fixIndentation(layout, existingIndentation, next.isEndOfScope, scope.indentation)
  
      trace("the resulting layout is %s", indentedLayout)
      
      (new StringFragment(indentedLayout), somethingChanged || indentedLayout != layout)
    }

    def innerMerge(scope: Scope): (List[Fragment], Boolean) = context("inner merger loop") {
      
      def processScope = (List[Fragment]() → false /: scope.children.zip(scope.children.tail)) {
        case ((fs, changes), (current: Scope, next)) => 
          val (resultingScope, changedScope) = innerMerge(current)
          val (layout, changed) = getLayoutBetween(current, next, scope)
          (fs ::: resultingScope ::: layout :: Nil, changes || changed || changedScope)
        case ((fs, changes), (current, next)) => 
          val (layout, changed) = getLayoutBetween(current, next, scope)
          (fs ::: current :: layout :: Nil, changes | changed)
      }
      
      
      def unchangedScope(scope: OriginalSourceFragment) = new Fragment {
        def print = scope.file.content.slice(scope.start, scope.end)
      } :: Nil
      
      def treeIsInChangeSet(s: Scope with WithTree) = hasTreeChanged(s.tree) || s.exists(_.isInstanceOf[ArtificialTreeFragment])

      trace("current scope is %s", scope)
      
      scope match {
        case scope: OriginalSourceFragment with WithTree if !treeIsInChangeSet(scope) => 
          trace("scope has not changed")
          (unchangedScope(scope), false)
        case scope => 
          trace("scope might have changed, "+ (scope match {
            case s: OriginalSourceFragment with WithTree if hasTreeChanged(s.tree) =>
              "is in changeset"
            case s: OriginalSourceFragment with WithTree if s.exists(_.isInstanceOf[ArtificialTreeFragment]) =>
              "contains artificial trees"
            case s: OriginalSourceFragment =>
              "does not contain a tree"
            case _ => 
              "not an OriginalSourceFragment"
          }))
          
          val(result, changes) = processScope

          (scope match {
            case scope: TreeScope if !changes =>
              trace("no changes found, keep scope")
              unchangedScope(scope)
            case _ =>
              trace("scope changed")
              result
          }, changes)
      }
    }
    
    innerMerge(rootScope)._1
  }
}
