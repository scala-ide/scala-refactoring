package scala.tools.refactor.printer

// cache, optimize, whatever!
class PartsHolder(root: ScopePart) {
  
  private def traverse(part: ScopePart, find: Part): Option[ScopePart] = {
    part.children foreach {
      case p if p == find => return Some(part)
      case p: ScopePart => traverse(p, find) match {
        case None => ()
        case Some(found) => return Some(found)
      }
      case _ => ()
    }
    None
  }
  
  def exists(part: Part) = traverse(root, part) match {
    case Some(found) => found.children.exists(_ == part)
    case None => false
  }
  
  def parentIndentation(part: Part) = traverse(root, part) match {
    case Some(found) => Some(found.indentation)
    case None => None//throw new Exception("parent not found")
  }
  
  def getNext(part: Part): Option[Triple[Part, List[Part], Part]] = {
    
//    println("get next after: "+ part)
   
    val neighbourhood = traverse(root, part).getOrElse(return None).children
    
    val partInOriginal = neighbourhood.dropWhile(_ != part)
    
    if(partInOriginal == Nil)
      return None
    
    val (whitespaceBetween, rest) = partInOriginal.tail.span(_.isWhitespace)
    
    if(rest == Nil)
      return None
      
    Some((partInOriginal.head, whitespaceBetween, rest.head))
  }
  
  def getPrevious(part: Part): Option[Triple[Part, List[Part], Part]] = {
    
//    println("get previous before: "+ part)
   
    val neighbourhood = traverse(root, part).getOrElse(return None).children
    
    val partInOriginal = neighbourhood.reverse.dropWhile(_ != part)
    
    if(partInOriginal == Nil)
      return None
              
    val (whitespaceBetween, rest) = partInOriginal.tail.span(_.isWhitespace)
    
    if(rest == Nil)
      return None
    
    Some((rest.head, whitespaceBetween.reverse, partInOriginal.head))
  }
}