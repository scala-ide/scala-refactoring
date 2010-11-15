/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

trait AbstractPrinter extends SourceCodeHelpers {
  
  this: common.Tracing with common.PimpedTrees with Indentations with common.CompilerAccess =>
  
  import global._
  
  trait ChangeSet {
    def hasChanged(t: Tree): Boolean
  }
  
  object AllTreesHaveChanged extends ChangeSet {
    def hasChanged(t: Tree) = true
  }
  
  private[sourcegen] def print(t: Tree, ind: Indentation, changeSet: ChangeSet): Fragment
  
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
      
      val child = if (parent.hasExistingCode) {
                
        getChildrenIndentation(parent, tree) match {
          case Some(childIndent) => 
            print(tree, ind.setTo(childIndent), changeSet)
          case None =>
            print(tree, ind.incrementDefault, changeSet)
        }
        
      } else {
        print(tree, ind, changeSet)
      }
      
      child match {
        case EmptyFragment => EmptyFragment
        case f => f ++ (f.post ++ after, before ++ f.pre)
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
      after: Requisite,
      isFirst: Boolean = true): Fragment = {
    
    // FIXME these two methods should be pushed down to the two printers where they can be implemented much simpler. 
    // FIXME also, indentation handling is still ugly.
    
    (trees match {
      case Nil => EmptyFragment
      case t :: rest => (printSingleTree(parent, t, ind, changeSet, indent, NoRequisite, NoRequisite), printManyTrees(parent, rest, ind, changeSet, indent, separator, NoRequisite, NoRequisite, false)) match {
        case (l, r) if l.asText == "" => r
        case (l, r) if r.asText == "" => l
        case (l, r) =>
        
          val fixedIndentationSeparator = {
            if(indent && parent.hasExistingCode && !rest.head.hasExistingCode && separator.getLayout.asText.startsWith("\n")) {
              Requisite.newline(ind.current + ind.defaultIncrement)
            } else {
              separator
            }
          }
        
          val lr = l.post(l.center ++ l.trailing, NoLayout)
          val rr = r.pre(NoLayout, r.leading ++ r.center)
          val mid: Layout = (lr ++ fixedIndentationSeparator ++ rr).toLayout
          Fragment(l.leading, mid, r.trailing) ++ (r.post, l.pre)
      }
    }) match {
      case EmptyFragment => EmptyFragment
      case f => 
        if(isFirst && indent && parent.hasExistingCode && !trees.head.hasExistingCode && separator.getLayout.asText.startsWith("\n")) {
          (Layout(ind.defaultIncrement) ++ f) ++ (after, before)
        } else {
          f ++ (after, before)
        }
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