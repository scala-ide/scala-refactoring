package scala.tools.refactor.printer

import scala.tools.nsc.ast.Trees

// cache, optimize, whatever!
class PartsHolder(root: ScopePart) {
    
  private def visit(part: ScopePart, find: Part): Option[ScopePart] = {
    part.children foreach {
      case p if p == find => return Some(part)
      case p: ScopePart => visit(p, find) match {
        case None => ()
        case Some(found) => return Some(found)
      }
      case _ => ()
    }
    None
  }    
  
  private def visit(part: ScopePart, find: Trees#Tree): Option[ScopePart] = {
    println("now at part: "+ part)
    part.children foreach { child =>
      println("child: "+ child)
      
      child match {
        case child: WithTree if child.tree.pos == find.pos => return Some(part)
        case _ =>
      }

      child match {
        case scope: ScopePart => 
        
        println("going down into: "+ scope)
        
          visit(scope, find) match {
            case Some(res) => return Some(res)
            case None =>
          }
        case _ =>
      }
    }
    None
  }
  
  def exists(part: Part) = visit(root, part) match {
    case Some(found) => found.children.exists(_ == part)
    case None => false
  }
  
  def scopeIndentation(part: Part) = visit(root, part) match {
    case Some(found) => Some(found.indentation)
    case None => None//throw new Exception("parent not found")
  }
  
  def scopeIndentation(tree: Trees#Tree) = visit(root, tree) match {
    case Some(found) => Some(found.indentation)
    case None => None//throw new Exception("parent not found")
  }
  
  def getNext(part: Part): Option[Triple[Part, List[Part], Part]] = {
    
//    println("get next after: "+ part)
   
    val neighbourhood = visit(root, part).getOrElse(return None).children
    
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
   
    val neighbourhood = visit(root, part).getOrElse(return None).children
    
    val partInOriginal = neighbourhood.reverse.dropWhile(_ != part)
    
    if(partInOriginal == Nil)
      return None
              
    val (whitespaceBetween, rest) = partInOriginal.tail.span(_.isWhitespace)
    
    if(rest == Nil)
      return None
    
    Some((rest.head, whitespaceBetween.reverse, partInOriginal.head))
  }
}