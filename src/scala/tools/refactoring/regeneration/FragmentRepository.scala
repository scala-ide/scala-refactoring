package scala.tools.refactoring.regeneration

import scala.tools.nsc.ast.Trees

// cache, optimize, whatever!
class FragmentRepository(root: Scope) {
    
  private def visit(part: Scope, find: Fragment): Option[Scope] = {
    part.children foreach {
      case p if p == find => return Some(part)
      case p: Scope => visit(p, find) match {
        case None => ()
        case Some(found) => return Some(found)
      }
      case _ => ()
    }
    None
  }
  
  private def visit(part: Scope, find: Trees#Tree): Option[Scope] = {
    part.children foreach { child =>
      child match {
        case child: WithTree if child.tree.pos == find.pos => return Some(part)
        case _ =>
      }

      child match {
        case scope: Scope => 
          visit(scope, find) match {
            case Some(res) => return Some(res)
            case None =>
          }
        case _ =>
      }
    }
    None
  }
  
  def exists(part: Fragment) = visit(root, part) match {
    case Some(found) => found.children.exists(_ == part)
    case None => false
  }
  
  def scopeIndentation(part: Fragment) = visit(root, part) match {
    case Some(found) => Some(found.indentation)
    case None => None//throw new Exception("parent not found")
  }
  
  def scopeIndentation(tree: Trees#Tree) = visit(root, tree) match {
    case Some(found) => Some(found.indentation)
    case None => None//throw new Exception("parent not found")
  }
  
  def getNext(part: Fragment): Option[Triple[Fragment, List[Fragment], Fragment]] = {
    
//    println("get next after: "+ part)
   
    val neighbourhood = visit(root, part).getOrElse(return None).children
    
    val partInOriginal = neighbourhood.dropWhile(_ != part)
    
    if(partInOriginal == Nil)
      return None
    
    val (layoutBetween, rest) = partInOriginal.tail.span(_.isLayout)
    
    if(rest == Nil)
      return None
      
    Some((partInOriginal.head, layoutBetween, rest.head))
  }
  
  def getPrevious(part: Fragment): Option[Triple[Fragment, List[Fragment], Fragment]] = {
    
//    println("get previous before: "+ part)
   
    val neighbourhood = visit(root, part).getOrElse(return None).children
    
    val partInOriginal = neighbourhood.reverse.dropWhile(_ != part)
    
    if(partInOriginal == Nil)
      return None
              
    val (layoutBetween, rest) = partInOriginal.tail.span(_.isLayout)
    
    if(rest == Nil)
      return None
    
    Some((rest.head, layoutBetween.reverse, partInOriginal.head))
  }
}