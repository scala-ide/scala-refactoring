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
/*
  def merge(original: List[Part], modified: List[Part]) = {
    
    val partsHolder = new PartsHolder(original)

    def whitespaceRightOf(part: Part) = splitWhitespaceBetween(partsHolder.nextPartToTheRight(part))._1
    def whitespaceLeftOf(part: Part)  = splitWhitespaceBetween(partsHolder.nextPartToTheLeft(part))._2
    
    def withWhitespace(parts: Pair[Part, Part]): List[Part] = {
      
      val(left, ws, right) = partsHolder.nextPartToTheRight(parts._1)
            
      if(right == parts._2) {
        explain("Whitespace ▒▒"+ (ws mkString "") +"▒▒ is between ▒▒"+ left +"▒▒ and ▒▒"+ right +"▒▒.")
        ws
      } else {
        // at this point, we know that the order of parts has been changed.
        // reset the stack and start collecting all opening braces.
        StringPart(whitespaceRightOf(parts._1) + whitespaceLeftOf(parts._2)) :: Nil
      }
    }
    
    (modified zip modified.tail) flatMap {
      case p @ (BeginOfFile(_), right) => withWhitespace(p)
      case p @ (left, right) => left :: withWhitespace(p)
    }
  }*/
  
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
