package scala.tools.refactor.printer

trait Merger {
  
  self: WhitespaceSplitter with TreePrinter =>

  private def explain(what: String) = println(what)
  
  def merge(part: ScopePart, original: ScopePart): List[Part] = {
    
    val partsHolder = new PartsHolder(original)
    
    def withWhitespace(current: Part, next: Part, parent: ScopePart): List[Part] = {
    
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
        
        if(completeWhitespace.contains('\n')) {
                    
          partsHolder.parentIndentation(next) match {
            case Some(indentation) => 
            
              val currentIndentation = SourceHelper.indentationLength(next)
              val originalParentIndentation = partsHolder.parentIndentation(next).getOrElse(0)
              
              val desiredRelativeIndentation = currentIndentation - originalParentIndentation
              val newIndentation = parent.indentation + desiredRelativeIndentation
              
              if(newIndentation != SourceHelper.indentationLength(next)) {
                println("need to indent "+ next)
                completeWhitespace = completeWhitespace.replaceAll("""(?ms)\n\s*""", "\n" + " " * newIndentation )
              }
              
            case None =>
              if(next.isEndOfScope) {
                completeWhitespace = completeWhitespace.replaceAll("""(?ms)\n\s*""", "\n" + " " * (parent.indentation))
              } else {
                completeWhitespace = completeWhitespace.replaceAll("""(?ms)\n\s*""", "\n" + " " * (parent.indentation + 2))
              }
          }
        }
        
        new StringPart(completeWhitespace) :: Nil
      }
    }
    
    def partFrom(part: Part) = part match {
      case part if partsHolder exists part => part
      case part: FlagPart => StringPart(part.print) copyRequirements part
      case part: WithTree => print(part)
      case part: WithRequirement => StringPart("<non-tree part: "+ part.print +">") copyRequirements part
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
