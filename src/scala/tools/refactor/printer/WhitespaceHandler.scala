package scala.tools.refactor.printer

trait WhitespaceHandler {

  def splitWhitespaceBetween(parts: Option[Triple[Fragment,List[Fragment],Fragment]]): Pair[String, String] = parts match {
    
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
