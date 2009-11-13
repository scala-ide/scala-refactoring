package scala.tools.refactor.printer

trait Merger {
  
  def splitWhitespaceBetween(parts: Triple[Part,List[Part],Part]): Pair[String, String] = {
          
    val OpeningBrace = """(.*?\()(.*)""".r
    val ClosingBrace = """(.*?)(\).*)""".r
    val Comma = """(.*?),\s*(.*)""".r
    val NewLine = """(?ms)(.*?\n)(.*)""".r
    // strip comments!
    
    println("Splitting whitespace between "+ parts._1 +" and "+ parts._3)
    
    val whitespace = parts._2 mkString ""
    
    ((parts._1, whitespace, parts._3) match {
      case(_, NewLine(l, r)     , _) => (l, r, "NewLine")
      case(_, OpeningBrace(l, r), _) => (l, r, "OpeningBrace")
      case(_, ClosingBrace(l, r), _) => (l, r, "ClosingBrace")
      case(_, Comma(l, r),        _) => (l, r, "Comma")
      case(_, s                 , _) => (s, "","NoMatch")
    }) match {
      case(l, r, why) => 
        println("Whitespace ▒▒"+ whitespace +"▒▒ partitions into ▒▒"+ l +"▒▒ and ▒▒"+ r +"▒▒ ("+ why +").")
        (l, r)
    }
  }
  
  private def explain(what: String) = println(what)
  
  def merge(part: CompositePart, original: CompositePart): List[Part] = {
    
    val partsHolder = new PartsHolder(original)
    
    def withWhitespace(current: Part, next: Part): List[Part] = {
    
      explain("get ws after part: "+ current)
      
      val (currentFound, wsFound, nextFound) = partsHolder getNext current
      
      if (next == nextFound) {
        explain("Whitespace ▒▒"+ (wsFound mkString "") +"▒▒ is between ▒▒"+ current +"▒▒ and ▒▒"+ next +"▒▒.")
        wsFound
      } else {
        StringPart(splitWhitespaceBetween((currentFound, wsFound, nextFound))._1 + splitWhitespaceBetween(partsHolder getPrevious next)._2) :: Nil
      }
    }
    
    def innerMerge(part: CompositePart): List[Part] = {
    
      val list: List[(Part, Part)] = (part.children zip part.children.tail)
      
      explain("traversing parts: "+ list)
      
      list flatMap {
        case (current: CompositePart, next) => innerMerge(current) ::: withWhitespace(current, next)
        case (current, next) => current :: withWhitespace(current, next)
      }
    }
    
    innerMerge(part)
  }

  def satisfyRequirements(parts: List[Part]): List[Part] = parts match {
    case Nil => Nil
    case (first: WithRequirement) :: second :: rest if first.hasRequirements => 
      val whitespace = second.print
      
      first :: second :: (first.postRequirements.foldRight(List[Part]()) {
        (required: String, ps: List[Part])  =>
          if(whitespace contains required) {
            ps
          } else {
            StringPart(required) :: ps
          }
      }) ::: satisfyRequirements(rest)
    case x :: xs => x :: satisfyRequirements(xs)
  }
}
