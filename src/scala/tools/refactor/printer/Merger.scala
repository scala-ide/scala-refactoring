package scala.tools.refactor.printer

trait Merger {
  
  def splitWhitespaceBetween(parts: Triple[Part,List[Part],Part]) = {
          
    val OpeningBrace = """(.*?\()(.*)""".r
    val ClosingBrace = """(.*?)(\).*)""".r
    val Comma = """(.*?),\s*(.*)""".r
    // strip comments!
    
    explain("Splitting whitespace between "+ parts._1 +" and "+ parts._3)
    
    val whitespace = parts._2 mkString ""
    
    ((parts._1, whitespace, parts._3) match {
      case(_, OpeningBrace(l, r), _) => (l, r)
      case(_, ClosingBrace(l, r), _) => (l, r)
      case(_, Comma(l, r),        _) => (l, r)
      case(_, s                 , _) => (s, "")
    }) match {
      case(l, r) => 
        explain("Whitespace ▒▒"+ whitespace +"▒▒ partitions into ▒▒"+ l +"▒▒ and ▒▒"+ r +"▒▒.")
        (l, r)
    }
  }
  
  private def explain(what: String) = ()// println(what)

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
        StringPart(whitespaceRightOf(parts._1) + whitespaceLeftOf(parts._2)) :: Nil
      }
    }
    
    (modified zip modified.tail) flatMap {
      case p @ (BeginOfFile(_), right) => withWhitespace(p)
      case p @ (left, right) => left :: withWhitespace(p)
    }
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
