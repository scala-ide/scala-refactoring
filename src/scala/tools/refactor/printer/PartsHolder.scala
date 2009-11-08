package scala.tools.refactor.printer

// cache, optimize, whatever!
class PartsHolder(parts: List[Part]) {
  
  def nextPartToTheRight(part: Part) = {
    val partInOriginal = parts.dropWhile(_ != part)
    
    val wsAfterPart = partInOriginal.tail
    val nextPart = wsAfterPart.dropWhile(_.isWhitespace).head
    val whitespaceBetween = wsAfterPart.takeWhile(_.isWhitespace)
    
    (partInOriginal.head, whitespaceBetween, nextPart)
  }
  
  def nextPartToTheLeft(part: Part) = {
    val partInOriginal = parts.reverse.dropWhile(_ != part)
    
    val wsAfterPart = partInOriginal.tail
    val nextPart = wsAfterPart.dropWhile(_.isWhitespace).head
    val whitespaceBetween = wsAfterPart.takeWhile(_.isWhitespace)
    
    (nextPart, whitespaceBetween.reverse, partInOriginal.head)
  }
}