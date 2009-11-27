package scala.tools.refactor.printer

import scala.tools.refactor.Tracing
import scala.collection.mutable.ListBuffer

trait WhitespaceHandler {
  self: Tracing =>
  
  def processRequisites(current: Fragment, whitespaceAfterCurrent: String, whitespaceBeforeNext: String, next: Fragment) = context("requisites") {
  
    trace("whitespace %s", whitespaceAfterCurrent + whitespaceBeforeNext)
    
    // check for overlapping whitespaces and requirements! => testSortWithJustOne
    def getRequisite(r: Requisite) = if(!(whitespaceAfterCurrent + whitespaceBeforeNext).contains(r.check)) r.write else ""
      
    def mapRequirements(rs: ListBuffer[Requisite]) = rs.map( getRequisite ) mkString ""

    using(whitespaceAfterCurrent + mapRequirements(current.requiredAfter) + whitespaceBeforeNext + mapRequirements(next.requiredBefore)) {
      trace("results in %s", _)
    }
  }
  
  def fixIndentation(whitespace: String, existingIndentation: Option[Tuple2[Int, Int]], isEndOfScope: Boolean, currentScopeIndentation: Int): String = context("fix indentation") {

    if(whitespace.contains('\n')) {
      
      def indentString(length: Int) = {
        whitespace.replaceAll("""(?ms)\n[\t ]*""", "\n" + (" " * length))
      }
      
      existingIndentation match {
        case Some((originalScopeIndentation, originalIndentation)) =>
          trace("this is a reused fragment")
          if(isEndOfScope) {
            if(whitespace.matches("""\s+""")) {
              trace("at the end of scope, only whitespace, don't indent")
              whitespace
            } else {
              trace("at the end of scope, take parent indentation %d ", currentScopeIndentation)
              indentString(currentScopeIndentation)
            }
          } else {
            val desiredRelativeIndentation = originalIndentation - originalScopeIndentation
            val newIndentation = currentScopeIndentation + desiredRelativeIndentation
            
            trace("original indentation was %d, original scope indentation was %d", originalIndentation, originalScopeIndentation)
            trace("new scope's indentation is %d → indent to %d", currentScopeIndentation, newIndentation)
            
            if(newIndentation != originalIndentation) indentString(newIndentation) else whitespace
          }
        case None =>
          trace("this is a new fragment")
        
          if(isEndOfScope) {
            trace("at the end of the scope, take scope's parent indentation %d", currentScopeIndentation)
            indentString(currentScopeIndentation)
          } else {
            println("new scope's indentation is %d → indent to %d ", currentScopeIndentation, currentScopeIndentation + 2)
            indentString(currentScopeIndentation + 2)
          }
      }
    } else whitespace
  }

  def splitWhitespaceBetween(parts: Option[Triple[Fragment,List[Fragment],Fragment]]) = parts match {
    
    case Some((left, whitespaceFragments, right)) =>
      context("split whitespace") {
        val OpeningBrace = """(.*?\()(.*)""".r
        val ClosingBrace = """(?ms)(.*?)(\).*)""".r
        val Comma = """(.*?),\s*(.*)""".r
        val NewLine = """(?ms)(.*?\n)(.*)""".r
        
        // strip comments!
        val whitespace = whitespaceFragments mkString ""

        trace("splitting whitespace %s between %s and %s", whitespace, left, right)
        
        ((left, whitespace, right) match {
          case(_, OpeningBrace(l, r), _) => (l, r, "OpeningBrace")
          case(_, ClosingBrace(l, r), _) => (l, r, "ClosingBrace")
          case(_, NewLine(l, r)     , _) => (l, r, "NewLine")
          case(_, Comma(l, r),        _) => (l, r, "Comma")
          case(_, s                 , _) => (s, "","NoMatch")
        }) match {
          case(l, r, why) => 
            trace("whitespace splits into %s and %s", l, r)
            (l, r)
        }
      }
    case None => ("", "")
  }
}
