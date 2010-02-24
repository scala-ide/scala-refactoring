package scala.tools.refactoring.regeneration

import scala.collection.mutable.HashMap
trait FragmentRepository {

  self: Fragments =>
  
  val global: scala.tools.nsc.interactive.Global
  
  // cache, optimize, whatever!
  class FragmentRepository(root: Scope) {
    
    private object ScopeFound extends Exception {
      var result: Scope = _
      def apply(s: Scope) {
        result = s
        throw this
      }
    }

    private def visit(find: Fragment) = {
    
      def innerVisit(s: Scope) {
        s.children foreach {
          case p if p == find => ScopeFound(s)
          case p: Scope => innerVisit(p)
          case _ => ()
        }
      }
    
      var result = None: Option[Scope]
      
      try {
        innerVisit(root)
      } catch {
        case ScopeFound => result = Some(ScopeFound.result)
      }
      
      result
    }
    
    private def visit(find: global.Tree): Option[Scope] = {
            
      def innerVisit(part: Scope) {
      
        part.children foreach { child =>
        
          if(child.isInstanceOf[WithTree]) {
            if(child.asInstanceOf[WithTree].tree.pos.sameRange(find.pos)) {
              ScopeFound(part)
            }
          }
            
          if(child.isInstanceOf[Scope]) {
            innerVisit(child.asInstanceOf[Scope])
          }
        }
      }
            
      var result = None: Option[Scope]
      
      try {
        innerVisit(root)
      } catch {
        case ScopeFound => result = Some(ScopeFound.result)
      }
      
      result
    }
    
    def exists(part: Fragment) = {
      visit(part) match {
        case Some(found) => found.children.exists(_ == part)
        case None => false
      }
    }
    
    def scopeIndentation(part: Fragment) = {
      visit(part) map (_.indentation)
    }
  
    def scopeIndentation(tree: global.Tree) = {
      visit(tree) map (_.indentation)
    }
    
    def getNext(part: Fragment) = {
      get(part, (xs => xs), (_, _))
    }
  
    def getPrevious(part: Fragment) = {
      get(part, (_.reverse), (_1, _2) => (_2, _1))
    }
  
    private def get(
        part: Fragment, 
        findPart: List[Fragment] => List[Fragment], 
        mkReturn: (OriginalSourceFragment, OriginalSourceFragment) => (OriginalSourceFragment, OriginalSourceFragment)):
      Option[(OriginalSourceFragment, OriginalSourceFragment)] = {
      
      val neighbourhood = visit(part).getOrElse(return None).children
      
      findPart(neighbourhood).dropWhile(_ != part) match {
        case partInOriginal :: next :: _ => 
          Some(mkReturn(partInOriginal.asInstanceOf[OriginalSourceFragment], next.asInstanceOf[OriginalSourceFragment]))
        case _ => None
      }
    }
  }
}