package scala.tools.refactor.printer

// cache, optimize, whatever!
class PartsHolder(root: CompositePart) {
  
  private def traverse(part: CompositePart, find: Part): List[Part] = {
    part.children foreach {
      case p if p == find => return part.children
      case p: CompositePart => val res = traverse(p, find) match {
        case Nil => ()
        case found => return found
      }
      case p => ()
    }
    Nil
  }
  
  
  def getNext(part: Part) = {
    
    println("get next after: "+ part)
   
    val neighbourhood = traverse(root, part)
    
    val partInOriginal = neighbourhood.dropWhile(_ != part)
    
    val (whitespaceBetween, rest) = partInOriginal.tail.span(_.isWhitespace)
    
    (partInOriginal.head, whitespaceBetween, rest.head)
  }
  
    
  def getPrevious(part: Part) = {
    
    println("get previous before: "+ part)
   
    val neighbourhood = traverse(root, part)
    
    val partInOriginal = neighbourhood.reverse.dropWhile(_ != part)
              
    val (whitespaceBetween, rest) = partInOriginal.tail.span(_.isWhitespace)
    
    (rest.head, whitespaceBetween.reverse, partInOriginal.head)
  }
  
  
  /*
  
  
  def nextPartToTheRight(part: Part): (Part, List[Part], Part) = part match {
    
    case p: CompositePart =>
      nextPartToTheRight(p.children.last)
    
    case _ =>
      val partInOriginal = parts.dropWhile(_ != part)
      
      val (whitespaceBetween, rest) = partInOriginal.tail.span(_.isWhitespace)
  
      (partInOriginal.head, whitespaceBetween, rest.head)
  }
  
  def nextPartToTheLeft(part: Part): (Part, List[Part], Part) = part match {
    
    case p: CompositePart =>
      nextPartToTheLeft(p.children.first)
    case _ =>
      val partInOriginal = parts.reverse.dropWhile(_ != part)
      
      val wsAfterPart = partInOriginal.tail
      val nextPart = wsAfterPart.dropWhile(_.isWhitespace).head
      val whitespaceBetween = wsAfterPart.takeWhile(_.isWhitespace)
      
      (nextPart, whitespaceBetween.reverse, partInOriginal.head)
  }*/
}