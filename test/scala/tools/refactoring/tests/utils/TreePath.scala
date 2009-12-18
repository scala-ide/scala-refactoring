package scala.tools.refactoring.tests.utils

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.ast._

trait TreePath {

  val global: scala.tools.nsc.Global
    
  import global._
  
  case class Filter(what: AnyRef)
  
  case class SubTree(name: String)

  case class TreePath(children: Seq[Tree]) {
    
    def /(member: SubTree): Seq[Tree] = children flatMap { tree =>
      tree.getClass.getMethod(member.name).invoke(tree) match {
        case tree: Tree => Some(tree)
        case _ => None
      }
    }
    
    def /(find: AnyRef): Seq[Tree] = {
      
      class FilterTree extends Traverser {
        val hits = new ListBuffer[Tree]
        override def traverse(t: Tree) {
          if (t.getClass.getSimpleName == simpleName(find))
            hits += t
          super.traverse(t)
        }
      }
     
      val f = new FilterTree
      children.foreach(f traverse)
      f.hits.toList
    }
    
    def /(filter: Filter) : Seq[Tree] = children filter (_.getClass.getSimpleName == simpleName(filter.what))
    
    def /(child: Int): Option[Tree] = {
      
      val i = if (child < 0 ) children.size + child else child
      
      if(0 <= i && i <= children.size)
        Some(children(i))
      else 
        None
    }
    
    private def simpleName(a: AnyRef) = a.getClass.getSimpleName.substring("Trees$".length).replace("$", "")
  }

  implicit def treeToNode(t: Tree) = TreePath(t :: Nil)
  implicit def treesToNode(ts: Seq[Tree]) = TreePath(ts)
  implicit def optionOfTreeToNode(o: Option[Tree]) = o match {
    case Some(t) => TreePath(t :: Nil)
    case None => TreePath(Nil)
  }
}
