package scala.tools.refactoring.regeneration

import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.util.Tracing

import java.util.regex._

trait Merger {
  self: LayoutHandler with TreePrinter with Tracing =>
  
  def merge(scope: Scope, allFragments: FragmentRepository): List[Fragment] = context("merge fragments") {
    
    def withLayout(current: Fragment, next: Fragment, scope: Scope): (Fragment, Boolean) = {
    
      val currentExists = allFragments exists current
      val originalNext  = allFragments getNext current
      
      val (layout, somethingChanged) = if(currentExists && originalNext.isDefined && originalNext.get._3 == next) {
        val layout = originalNext.get._2 mkString ""
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
          if(currentExists) {
            splitLayoutBetween(allFragments getNext current)
          } else ("", "")
        
        /*
         * We also need the layout of our right neighbour:
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
	    
	    val existingIndentation = allFragments.scopeIndentation(next) flatMap (s => SourceHelper.indentationLength(next) map (s → _))
	    
	    val indentedLayout = fixIndentation(layout, existingIndentation, next.isEndOfScope, scope.indentation)
  
      trace("the resulting layout is %s", indentedLayout)
      
      (new StringFragment(indentedLayout), somethingChanged || indentedLayout != layout)
    }

    def innerMerge(scope: Scope): (List[Fragment], Boolean) = context("inner merger loop") {
              
      def printFragment(f: Fragment) = f match {
        case f if allFragments exists f => f
        case f: FlagFragment => StringFragment(f.print) copyRequirements f
        case f: WithTree => print(f)
        case f: WithRequisite => StringFragment("") copyRequirements f
        case _ => StringFragment("<non-tree part>")
      }
      
      trace("current scope is %s", scope)

      val(result, changes) = (scope.children zip scope.children.tail).foldLeft((List[Fragment](), false)) {
        case ((fs, changes), (current: Scope, next)) => 
          val (resultingScope, changedScope) = innerMerge(current)
          val (layout, changed) = withLayout(current, next, scope)
          (fs ::: resultingScope ::: layout :: Nil, changes || changed || changedScope)
        case ((fs, changes), (current, next)) => 
          val (layout, changed) = withLayout(current, next, scope)
          (fs ::: printFragment(current) :: layout :: Nil, changes | changed)
      }

      (scope match {
        case scope: TreeScope if !changes =>
          trace("no changes found, keep scope")
          List(new Fragment {
            def print = scope.file.content.slice(scope.start, scope.end)
          })
        case _ => 
          trace("scope changed")
          result
      }, changes)
    }
    
    innerMerge(scope)._1
  }
}
