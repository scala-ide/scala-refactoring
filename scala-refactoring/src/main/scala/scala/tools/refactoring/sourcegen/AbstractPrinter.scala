package scala.tools.refactoring
package sourcegen

trait AbstractPrinter extends SourceCodeHelpers {
  
  this: common.Tracing with common.PimpedTrees with Indentations =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  trait ChangeSet {
    def hasChanged(t: Tree): Boolean
  }
  
  def print(t: Tree, ind: Indentation, changeSet: ChangeSet): Fragment
  
  private[sourcegen] def printSingleTree(
      parent: Tree,
      tree: Tree, 
      ind: Indentation,
      changeSet: ChangeSet,
      indent: Boolean,
      before: Requisite,
      after: Requisite): Fragment = {

    if(indent && tree.hasExistingCode && parent.isInstanceOf[If] && tree.pos.isRange) {
      print(tree, ind.setTo(indentation(tree)), changeSet) match {
        case EmptyFragment => EmptyFragment
        case f => 
          val x = f
          f ++ (after, before)
      }
    } else if(indent) {
      
      val (child, leading) = if (parent.hasExistingCode) {
                
        val child = getChildrenIndentation(parent, tree) match {
          case Some(childIndent) => 
            print(tree, ind.setTo(childIndent), changeSet)
          case None =>
            print(tree, ind.incrementDefault, changeSet)
        }
        
        val leading = if(tree.hasExistingCode) NoLayout else Layout(ind.defaultIncrement)
        
        child → leading
        
      } else {
        print(tree, ind, changeSet) → NoLayout
      }
      
      child match {
        case EmptyFragment => EmptyFragment
        case f => Fragment(leading ++ f.leading , f.center, f.trailing) ++ (f.post ++ after, before ++ f.pre)
      }
       
    } else {
      print(tree, ind.setTo(getChildrenIndentation(parent, tree) getOrElse ind.current), changeSet) match {
        case EmptyFragment => EmptyFragment
        case f => f ++ (after, before)
      }
    }
  }
  
  private[sourcegen] def printManyTrees(
      parent: Tree,
      trees: List[Tree], 
      ind: Indentation, 
      changeSet: ChangeSet,
      indent: Boolean,
      separator: Requisite,
      before: Requisite,
      after: Requisite): Fragment = {
    (trees match {
      case Nil => EmptyFragment
      case t :: rest => (printSingleTree(parent, t, ind, changeSet, indent, NoRequisite, NoRequisite), printManyTrees(parent, rest, ind, changeSet, indent, separator, NoRequisite, NoRequisite)) match {
        case (l, r) if l.asText == "" => r
        case (l, r) if r.asText == "" => l
        case (l, r) =>
          val mid: Layout = (l.post(l.center ++ l.trailing, NoLayout) ++ separator ++ r.pre(NoLayout, r.leading ++ r.center)).toLayout
          Fragment(l.leading, mid, r.trailing) ++ (r.post, l.pre)
      }
    }) match {
      case EmptyFragment => EmptyFragment
      case f => f ++ (after, before)
    }
  }
  
  private def getChildrenIndentation(p: Tree, t: Tree): Option[String] = {
    if (p.hasExistingCode && !(p.isInstanceOf[If] && t.isInstanceOf[Block])/*prevent that we indent too much*/) {
    
      val childrenOnNewLine = children(p) filter (_.pos.isRange) filter (_.pos.line != p.pos.line)
      
      if(childrenOnNewLine exists (_ samePos t)) {
        Some(indentation(t))
      } else {
        childrenOnNewLine.headOption map indentation
      }
       
    } else None
  }
}