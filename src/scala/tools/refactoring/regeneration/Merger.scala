package scala.tools.refactoring.regeneration

import scala.tools.refactoring.Tracing

import java.util.regex._

trait Merger {
  self: LayoutHandler with TreePrinter with Tracing =>
  
  def merge(scope: Scope, allFragments: FragmentRepository): List[Fragment] = context("merge fragments") {
    
    def withLayout(current: Fragment, next: Fragment, scope: Scope): List[Fragment] = {
    
      val currentExists = allFragments exists current
      
      /*if(currentExists && allFragments.getNext(current).get._3 == next) {
        val (_, wsFound, nextFound) = (allFragments getNext current).get //FIXME
        //explain("Layout ▒▒"+ (wsFound mkString "") +"▒▒ is between ▒▒"+ current +"▒▒ and ▒▒"+ next +"▒▒.")
        wsFound
      } else */{
        /*
         * The tree has been re-arranged, the next part in the original source isn't our current next. 
         * We have to split the layout between our current part and its next part in the original 
         * source
         * */
        val (layoutAfterCurrent, _) = 
          if(currentExists) {
            splitLayoutBetween(allFragments getNext current)
          } else {
            //("<current does not exist>", "<current does not exist>")
            ("", "")
          }
        
        /*
         * We also need the layout of our right neighbour:
         * */
        val (_, layoutBeforeNext) = 
          if(allFragments exists next) {
            splitLayoutBetween(allFragments getPrevious next)
          } else {
            //("<next does not exist>", "<next does not exist>")
            ("", "")
          }
        
        /*
         * We now have 4 fragments of layout, we only need the left slice of our current fragment (that is, 
         * the layout that is adjacent to the current fragment) and the (right) slice of layout that is directly
         * before the next fragment in the tree. Combined, we get all layout we need.
         * */
        
        /*
         * Fragments may define required strings that need to be present in the layout before or after them.
         * */
        
        val layout = processRequisites(current, layoutAfterCurrent, layoutBeforeNext, next)
        
		    trace("layout is %s", layout)
		    trace("the next fragment is %s", next)
		    
		    val existingIndentation = allFragments.scopeIndentation(next) flatMap (s => SourceHelper.indentationLength(next) map (s → _))
		    
		    val indentedLayout = fixIndentation(layout, existingIndentation, next.isEndOfScope, scope.indentation)
    
        trace("the resulting layout is %s", indentedLayout)
        
        new StringFragment(indentedLayout) :: Nil
      }
    }
    
    def printFragment(f: Fragment) = f match {
      case f if allFragments exists f => f
      case f: FlagFragment => StringFragment(f.print) copyRequirements f
      case f: WithTree => print(f)
      case f: WithRequisite => StringFragment("") copyRequirements f
      case _ => StringFragment("<non-tree part>")
    }

    def innerMerge(scope: Scope): List[Fragment] = context("inner merger loop") {
      trace("current scope is %s", scope)
      (scope.children zip scope.children.tail) flatMap {
        case (current: Scope, next) => innerMerge(current) ::: withLayout(current, next, scope)
        case (current, next) => printFragment(current) :: withLayout(current, next, scope)
      }
    }
    
    innerMerge(scope)
  }
}
