package scala.tools.refactoring.common

object TracingHelpers {
  def compactify(text: String): String = {
    val lines = text.lines.toArray
    val firstLine = lines.headOption.getOrElse("")
    val snipAfter = 50

    val (compactedFirstLine, dotsAdded) = {
      if (firstLine.size <= snipAfter) {
        (firstLine, false)
      } else {
        (firstLine.substring(0, snipAfter) + "...", true)
      }
    }
    
    if (lines.size <= 1) {
      compactedFirstLine
    } else {
      val compactedFirstLineWithDots = {
        if (dotsAdded) compactedFirstLine
        else compactedFirstLine + "..."
      }
      
      val moreLines = lines.size - 1
      compactedFirstLineWithDots + s"($moreLines more lines ommitted)"
    }
  }
  
  def toCompactString(any: Any): String = compactify("" + any)
}