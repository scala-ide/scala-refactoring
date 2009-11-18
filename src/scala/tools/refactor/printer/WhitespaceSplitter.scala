package scala.tools.refactor.printer

trait WhitespaceSplitter {

  def splitWhitespaceBetween(parts: Option[Triple[Part,List[Part],Part]]): Pair[String, String] = parts match {
    
    case Some((left, whitespaceParts, right)) =>
          
      val OpeningBrace = """(.*?\()(.*)""".r
      val ClosingBrace = """(.*?)(\).*)""".r
      val Comma = """(.*?),\s*(.*)""".r
      val NewLine = """(?ms)(.*?\n)(.*)""".r
      // strip comments!
      
      //explain("Splitting whitespace between "+ left +" and "+ right)
      
      val whitespace = whitespaceParts mkString ""
      
      ((left, whitespace, right) match {
        case(_, NewLine(l, r)     , _) => (l, r, "NewLine")
        case(_, OpeningBrace(l, r), _) => (l, r, "OpeningBrace")
        case(_, ClosingBrace(l, r), _) => (l, r, "ClosingBrace")
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
