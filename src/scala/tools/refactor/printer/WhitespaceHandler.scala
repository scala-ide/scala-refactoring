package scala.tools.refactor.printer

import scala.collection.mutable.ListBuffer

trait WhitespaceHandler {
  
  def processRequisites(current: Fragment, whitespaceAfterCurrent: String, whitespaceBeforeNext: String, next: Fragment) = {
  
    // check for overlapping whitespaces and requirements! => testSortWithJustOne
    def getRequisite(r: Requisite) = if(!(whitespaceAfterCurrent + whitespaceBeforeNext).contains(r.check)) r.write else ""
      
    def mapRequirements(rs: ListBuffer[Requisite]) = rs.map( getRequisite ) mkString ""

    whitespaceAfterCurrent + mapRequirements(current.requiredAfter) + whitespaceBeforeNext + mapRequirements(next.requiredBefore)
  }
  
  def fixIndentation(whitespace: String, allFragments: FragmentRepository, next: Fragment, scope: Scope) = {
    
    if(whitespace.contains('\n')) {
      
      def indentString(length: Int) = {
        // exclude comments
        whitespace.replaceAll("""(?ms)\n[\t ]*""", "\n" + (" " * length))
      }
      
      println("\n==============\nWhitespace contains newlines: «"+ whitespace +"» and we are currently handling: "+ next)
                
      allFragments.scopeIndentation(next) match {
        case Some(originalscopeIndentation) => 
        
          if(next.isEndOfScope) {
            
            println("at the end of scope, so we take the parent's indentation: "+ (scope.indentation))
            if(whitespace.matches("""\s+"""))                  
              whitespace // if its just spaces, then leave it alone
            else
              indentString(scope.indentation)

          } else {
          
            val currentIndentation = SourceHelper.indentationLength(next)
            println("Our original indentation was: "+ currentIndentation)
            println("Our original scope's indentation was: "+ originalscopeIndentation)
            val desiredRelativeIndentation = currentIndentation - originalscopeIndentation
            println("Relative to our scope, we need an indentation of: "+ desiredRelativeIndentation)
            val newIndentation = scope.indentation + desiredRelativeIndentation
            println("Our new scope's indentation is: "+ scope.indentation)
            println("This means we want an indentation of : "+ newIndentation)
            
            if(newIndentation != currentIndentation) {
              println("need to indent "+ next)
              indentString(newIndentation)
            } else whitespace
          }
          
        case None =>
        
          println("We are a new node! "+ next)
        
          if(next.isEndOfScope) {
            println("at the end of the scope, so we want the scope's parent indentation")
              scope.parent match {
                case Some(p) => 
                  println(", that is: "+ (p.indentation + 2))
                  indentString(p.indentation + 2)
                case None => 
                  println(", oh, no parent, then 0")
                  indentString(0)
              }
          } else {
            println("our scope has an indentation of: "+ scope.indentation)
            println("and we want to be indented, to: "+ (scope.indentation + 2))
            indentString(scope.indentation + 2)
          }
      }
    } else whitespace
  }

  def splitWhitespaceBetween(parts: Option[Triple[Fragment,List[Fragment],Fragment]]) = parts match {
    
    case Some((left, whitespaceFragments, right)) =>
          
      val OpeningBrace = """(.*?\()(.*)""".r
      val ClosingBrace = """(?ms)(.*?)(\).*)""".r
      val Comma = """(.*?),\s*(.*)""".r
      val NewLine = """(?ms)(.*?\n)(.*)""".r
      // strip comments!
      
      //explain("Splitting whitespace between "+ left +" and "+ right)
      
      val whitespace = whitespaceFragments mkString ""
      
      ((left, whitespace, right) match {
        case(_, OpeningBrace(l, r), _) => (l, r, "OpeningBrace")
        case(_, ClosingBrace(l, r), _) => (l, r, "ClosingBrace")
        case(_, NewLine(l, r)     , _) => (l, r, "NewLine")
        case(_, Comma(l, r),        _) => (l, r, "Comma")
        case(_, s                 , _) => (s, "","NoMatch")
      }) match {
        case(l, r, why) => 
          println("Whitespace ▒▒"+ whitespace.replace("\n", "\\n") +"▒▒ partitions into ▒▒"+ l.replace("\n", "\\n") +"▒▒ and ▒▒"+ r.replace("\n", "\\n") +"▒▒ ("+ why +").")
          (l, r)
      }
    case None => ("", "")
  }
}
