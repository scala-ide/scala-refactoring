package scala.tools.refactor.printer

trait Merger {
  
  self: WhitespaceSplitter with TreePrinter =>

  private def explain(what: String) = println(what)
  
  def merge(part: CompositePart, original: CompositePart): List[Part] = {
    
    val partsHolder = new PartsHolder(original)
    
    def withWhitespace(current: Part, next: Part): List[Part] = {
    
      explain("get ws after part: "+ current)
      
      /*if(partsHolder.exists(current) && partsHolder.getNext(current).get._3 == next) {
        val (_, wsFound, nextFound) = (partsHolder getNext current).get //FIXME
        explain("Whitespace ▒▒"+ (wsFound mkString "") +"▒▒ is between ▒▒"+ current +"▒▒ and ▒▒"+ next +"▒▒.")
        wsFound
      } else */{
        /*
         * The tree has been re-arranged, the next part in the original source isn't our current next. 
         * We have to split the whitespace between our current part and its next part in the original 
         * source
         * */
        val (whitespaceAfterCurrent, _) = 
          if(partsHolder exists current) {
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
        
        new StringPart(whitespaceAfterCurrent + wsWithReqs1 + whitespaceBeforeNext + wsWithReqs2) with Whitespace :: Nil
      }
    }
    
    def partFrom(part: Part) = part match {
      case part if partsHolder exists part => part
      case part: FlagPart => StringPart(part.print) copyRequirements part
      case part: WithTree => print(part)
      case part: WithRequirement => StringPart("<non-tree part: "+ part.print +">") copyRequirements part
      case _ => StringPart("<non-tree part>")
    }
    
    def innerMerge(part: CompositePart): List[Part] = {
    
      val list: List[(Part, Part)] = (part.children zip part.children.tail)
      
      explain("traversing parts: "+ list)
      
      list flatMap {
        case (current: CompositePart, next) => innerMerge(current) ::: withWhitespace(current, next)
        // do we need the end-of-scope thing? case (current, next: CompositePart#EndOfScope) => partFrom(current) :: withWhitespace(current, next) ::: next :: Nil
        case (current, next) => partFrom(current) :: withWhitespace(current, next)
      }
    }
    
    innerMerge(part)
  }
}
