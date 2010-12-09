/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

trait AbstractPrinter extends SourceCodeHelpers {
    
  this: common.Tracing with common.PimpedTrees with Indentations with common.CompilerAccess =>
  
  import global._
  
  case class PrintingContext(ind: Indentation, changeSet: ChangeSet, parent: Tree)
  
  trait ChangeSet {
    def hasChanged(t: Tree): Boolean
  }
  
  object AllTreesHaveChanged extends ChangeSet {
    def hasChanged(t: Tree) = true
  }
  
  private[sourcegen] def print(t: Tree, ind: Indentation, changeSet: ChangeSet): Fragment
  
  private[sourcegen] def printIndentedSingleTree(
      tree: Tree,
      before: Requisite,
      after: Requisite)(implicit ctx: PrintingContext): Fragment = {

    import ctx._
    
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
    
    child ifNotEmpty (f => f ++ (f.post ++ after, before ++ f.pre))  
  }
  
  private[sourcegen] def printSingleTree(
      tree: Tree,
      before: Requisite,
      after: Requisite)(implicit ctx: PrintingContext): Fragment = {

    import ctx._
    
    val newIndent = ind.setTo(getChildrenIndentation(parent, tree) getOrElse ind.current)
    
    print(tree, newIndent, changeSet) ifNotEmpty (_ ++ (after, before))
  }
  
  private[sourcegen] def printIndentedManyTrees(
      trees: List[Tree], 
      separator: Requisite,
      before: Requisite,
      after: Requisite,
      isFirst: Boolean = true)(implicit ctx: PrintingContext): Fragment = {
    
    import ctx._
    
    (trees match {
      case Nil => EmptyFragment
      case t :: rest => (printIndentedSingleTree(t, NoRequisite, NoRequisite), printIndentedManyTrees(rest, separator, NoRequisite, NoRequisite, false)) match {
        case (l, r) if l.asText == "" => r
        case (l, r) if r.asText == "" => l
        case (l, r) =>
        
          val fixedIndentationSeparator = {
            if(parent.hasExistingCode && !rest.head.hasExistingCode && separator.getLayout.asText.startsWith("\n")) {
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
    }) ifNotEmpty { f => 
      if(isFirst && parent.hasExistingCode && !trees.head.hasExistingCode && separator.getLayout.asText.startsWith("\n")) {
        (Layout(ind.defaultIncrement) ++ f) ++ (after, before)
      } else {
        f ++ (after, before)
      }
    }
  }
  
  private[sourcegen] def printManyTrees(
      trees: List[Tree], 
      separator: Requisite,
      before: Requisite,
      after: Requisite,
      isFirst: Boolean = true)(implicit ctx: PrintingContext): Fragment = {
    
    import ctx._
    
    (trees match {
      case Nil => EmptyFragment
      case t :: rest => (printSingleTree(t, NoRequisite, NoRequisite), printManyTrees(rest, separator, NoRequisite, NoRequisite, false)) match {
        case (l, r) if l.asText == "" => r
        case (l, r) if r.asText == "" => l
        case (l, r) =>
        
          val lr = l.post(l.center ++ l.trailing, NoLayout)
          val rr = r.pre(NoLayout, r.leading ++ r.center)
          val mid: Layout = (lr ++ separator ++ rr).toLayout
          Fragment(l.leading, mid, r.trailing) ++ (r.post, l.pre)
      }
    }) ifNotEmpty ( _ ++ (after, before))
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