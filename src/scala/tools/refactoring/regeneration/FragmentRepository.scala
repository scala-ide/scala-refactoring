package scala.tools.refactoring.regeneration

trait FragmentRepository {

  self: Fragments =>
  
  val global: scala.tools.nsc.Global
  
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
    
    private def visit(part: Scope, find: global.Tree): Option[Scope] = {
      
      var found: Option[Scope] = None
      
      def innerVisit(part: Scope) {
      
        part.children foreach { child =>
        
          if(child.isInstanceOf[WithTree]) {
            if(child.asInstanceOf[WithTree].tree.pos.sameRange(find.pos) && found == None) {
              found = Some(part)
            }
          }
            
          if(child.isInstanceOf[Scope] && found == None) {
            innerVisit(child.asInstanceOf[Scope])
          }
        }
      }
      
      innerVisit(part)
      
      return found
    }
    
    def exists(part: Fragment) = visit(root, part) match {
      case Some(found) => found.children.exists(_ == part)
      case None => false
    }
    
    def scopeIndentation(part: Fragment) = visit(root, part) map (_.indentation)
  
    def scopeIndentation(tree: global.Tree) = {
      val treeScope = visit(root, tree)
      if(tree.isInstanceOf[global.Block]) {
        val treeScope = visit(root, tree)
        val i = treeScope.get.indentation
        ()
      }
      treeScope map (_.indentation)
    }
    
    def getNext(part: Fragment) = get(part, (xs => xs), (_, _, _))
  
    def getPrevious(part: Fragment) = get(part, (_.reverse), (_1, _2, _3) => (_3, _2.reverse, _1))
  
    private def get(part: Fragment, findPart: List[Fragment] => List[Fragment], mkReturn: (Fragment, List[Fragment], Fragment) => Triple[Fragment, List[Fragment], Fragment]): Option[Triple[Fragment, List[Fragment], Fragment]] = {
      
      val neighbourhood = visit(root, part).getOrElse(return None).children
      
      val partInOriginal = findPart(neighbourhood).dropWhile(_ != part)
      
      if(partInOriginal == Nil)
        return None
      
      val (layoutBetween, rest) = partInOriginal.tail.span(_.isLayout)
      
      if(rest == Nil)
        return None
        
      Some(mkReturn(partInOriginal.head, layoutBetween, rest.head))
    }
  }
}