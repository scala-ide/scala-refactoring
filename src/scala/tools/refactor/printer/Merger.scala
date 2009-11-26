package scala.tools.refactor.printer

import java.util.regex._

trait Merger {
  
  self: WhitespaceSplitter with TreePrinter =>

  private def explain(what: String) = println(what)
  
  def merge(part: ScopePart, partsHolder: PartsHolder): List[Part] = {
    
    def withWhitespace(current: Part, next: Part, scope: ScopePart): List[Part] = {
    
      //explain("get ws after part: "+ current)
      
      val currentExists = partsHolder exists current
      
      /*if(currentExists && partsHolder.getNext(current).get._3 == next) {
        val (_, wsFound, nextFound) = (partsHolder getNext current).get //FIXME
        //explain("Whitespace ▒▒"+ (wsFound mkString "") +"▒▒ is between ▒▒"+ current +"▒▒ and ▒▒"+ next +"▒▒.")
        wsFound
      } else */{
        /*
         * The tree has been re-arranged, the next part in the original source isn't our current next. 
         * We have to split the whitespace between our current part and its next part in the original 
         * source
         * */
        val (whitespaceAfterCurrent, _) = 
          if(currentExists) {
            splitWhitespaceBetween(partsHolder getNext current)
          } else {
            //("<current does not exist>", "<current does not exist>")
            ("", "")
          }
        
        /*
         * We also need the whitespace of our right neighbour:
         * */
        val (_, whitespaceBeforeNext) = 
          if(partsHolder exists next) {
            splitWhitespaceBetween(partsHolder getPrevious next)
          } else {
            //("<next does not exist>", "<next does not exist>")
            ("", "")
          }
        
        /*
         * We now have 4 parts of whitespace, we only need the left slice of our current part (that is, 
         * the whitespace that is adjacent to the current part) and the (right) slice of whitespace that is directly
         * before the next part in the tree. Combined, we get all whitespace we need.
         * */
        
        val allWhitespace = whitespaceAfterCurrent + whitespaceBeforeNext
                
        // check for overlapping whitespaces and requirements! => testSortWithJustOne
        val wsWithReqs1 = current.postRequirements.map( req => if(!allWhitespace.contains(req.check)) req.write else ""  ) mkString ""
        val wsWithReqs2 = next.preRequirements.map( req => if(!allWhitespace.contains(req.check)) req.write else ""  ) mkString ""
        
        var completeWhitespace = whitespaceAfterCurrent + wsWithReqs1 + whitespaceBeforeNext + wsWithReqs2
        
        def indentString(ws: String, length: Int) = {
          
          val parts = ws.split("""(?ms)\n\s*""")
          val replaceStr = "\n" + (" " * length)
          
          val result = parts mkString replaceStr
          
          /*if(ws.matches("""(?ms).+?\n$"""))
            result + "\n" 
          else */if(ws.matches("""(?ms).*?\n\s*$"""))
            result + replaceStr
          else
            result
          
          ws.replaceAll("""(?ms)\n[\t ]*""", "\n" + (" " * length))
        }
        
        
        if(completeWhitespace.contains('\n')) {
          
          println("\n==============\nWhitespace contains newlines: «"+ completeWhitespace +"» and we are currently handling: "+ next)
                    
          partsHolder.scopeIndentation(next) match {
            case Some(originalscopeIndentation) => 
            
              if(next.isEndOfScope) {
                
                println("at the end of scope, so we take the parent's indentation: "+ (scope.indentation))
                if(completeWhitespace.matches("""\s+"""))                  
                  () // if its just spaces, then leave it alone
                else
                  completeWhitespace = indentString(completeWhitespace, scope.indentation)

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
                  completeWhitespace = indentString(completeWhitespace, newIndentation)
                }
              }
              
            case None =>
            
              println("We are a new node! "+ next)
            
              if(next.isEndOfScope) {
                println("at the end of the scope, so we want the scope's parent indentation")
                  scope.parent match {
                    case Some(p) => 
                      println(", that is: "+ (p.indentation + 2))
                      completeWhitespace = indentString(completeWhitespace, p.indentation + 2)
                    case None => 
                      println(", oh, no parent, then 0")
                      completeWhitespace = indentString(completeWhitespace, 0)
                  }
              } else {
                println("our scope has an indentation of: "+ scope.indentation)
                println("and we want to be indented, to: "+ (scope.indentation + 2))
                completeWhitespace = indentString(completeWhitespace, scope.indentation + 2)
              }
          }
        }
        println("the resulting whitespace is thus: «"+ completeWhitespace +"»")
        /* else if (next.isEndOfScope && completeWhitespace.contains('}')){
          println("\n==============\nWS contains } but no linebreak: «"+ completeWhitespace +"»")
          scope.parent match {
            case Some(p) => 
              println(", that is: "+ p.indentation)
              completeWhitespace = completeWhitespace.replaceAll("""^\s*""", " " * (p.indentation))
            case None => 
              println(", oh, no parent, then 0")
              completeWhitespace = completeWhitespace.replaceAll("""^\s*""", "")
          }
        }*/
        
        new StringPart(completeWhitespace) :: Nil
      }
    }
    
    def partFrom(part: Part) = part match {
      case part if partsHolder exists part => part
      case part: FlagPart => StringPart(part.print) copyRequirements part
      case part: WithTree => print(part)
      case part: WithRequirement => StringPart("") copyRequirements part
      case _ => StringPart("<non-tree part>")
    }
    
    def innerMerge(part: ScopePart): List[Part] = {
    
      val list: List[(Part, Part)] = (part.children zip part.children.tail)
      
      //explain("traversing parts: "+ list)
      
      list flatMap {
        case (current: ScopePart, next) => innerMerge(current) ::: withWhitespace(current, next, part)
        // do we need the end-of-scope thing? case (current, next: CompositePart#EndOfScope) => partFrom(current) :: withWhitespace(current, next) ::: next :: Nil
        case (current, next) => partFrom(current) :: withWhitespace(current, next, part)
      }
    }
    
    innerMerge(part)
  }
}
