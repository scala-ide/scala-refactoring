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
  
  def exists(part: Part) = traverse(root, part).exists(_ == part)
  
  def getNext(part: Part) = {
    
//    println("get next after: "+ part)
   
    val neighbourhood = traverse(root, part)
    
    val partInOriginal = neighbourhood.dropWhile(_ != part)
    
    val (whitespaceBetween, rest) = partInOriginal.tail.span(_.isWhitespace)
    
    (partInOriginal.head, whitespaceBetween, rest.head)
  }
  
  def getPrevious(part: Part) = {
    
//    println("get previous before: "+ part)
   
    val neighbourhood = traverse(root, part)
    
    val partInOriginal = neighbourhood.reverse.dropWhile(_ != part)
              
    val (whitespaceBetween, rest) = partInOriginal.tail.span(_.isWhitespace)
    
    (rest.head, whitespaceBetween.reverse, partInOriginal.head)
  }
}