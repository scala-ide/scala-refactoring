package scala.tools.refactoring
package sourcegen

trait AbstractPrinter extends SourceCodeHelpers {
  
  this: common.Tracing with common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def printSingleTree(
      parent: Tree,
      tree: Tree, 
      ind: Indentation, 
      continue: (Tree, Indentation) => Option[Fragment],
      indent: Boolean,
      before: Requisite,
      after: Requisite): Fragment = {

    if(indent && tree.hasExistingCode && parent.isInstanceOf[If] && tree.pos.isRange) {
      ind.setTo(indentation(tree)) {
        continue(tree, ind) match {
          case Some(f) => 
            val x = f
            f ++ (after, before)
          case None => 
            EmptyFragment
        }
      }
    } else if(indent) {
      
      val (child: Fragment, leading: Layout) = if (parent.hasExistingCode) {
                
        val child: Fragment = getChildrenIndentation(parent, tree) match {
          case Some(childIndent) => 
            ind.setTo(childIndent) {
              continue(tree, ind) getOrElse EmptyFragment
            }
          case None =>
            ind.default {
              continue(tree, ind) getOrElse EmptyFragment
            }
        }
        
        val leading = if(tree.hasExistingCode) NoLayout else Layout(ind.defaultIncrement)
        
        child → leading
        
      } else {
        (continue(tree, ind) getOrElse EmptyFragment) → NoLayout
      }
      
      child match {
        case EmptyFragment => EmptyFragment
        case f => Fragment(leading ++ f.leading , f.center, f.trailing) ++ (f.post ++ after, before ++ f.pre)
      }
       
    } else {
      ind.setTo(getChildrenIndentation(parent, tree) getOrElse ind.text) {
        continue(tree, ind) match {
          case Some(f) => 
            val x = f
            f ++ (after, before)
          case None => 
            EmptyFragment
        }
      }
    }
  }
  
  def printManyTrees(
      parent: Tree,
      trees: List[Tree], 
      ind: Indentation, 
      continue: (Tree, Indentation) => Option[Fragment], 
      indent: Boolean = false,
      separator: Requisite,
      before: Requisite,
      after: Requisite): Fragment = {
    (trees match {
      case Nil => EmptyFragment
      case t :: rest => (printSingleTree(parent, t, ind, continue, indent, NoRequisite, NoRequisite), printManyTrees(parent, rest, ind, continue, indent, separator, NoRequisite, NoRequisite)) match {
        case (l, r) if l.asText == "" => r
        case (l, r) if r.asText == "" => l
        case (l, r) =>
          val mid: Layout = (l.post.validate(l.center ++ l.trailing, NoLayout) ++ separator ++ r.pre.validate(NoLayout, r.leading ++ r.center)).toLayout
          Fragment(l.leading, mid, r.trailing) ++ (r.post, l.pre)
      }
    }) match {
      case EmptyFragment => EmptyFragment
      case f => 
      val x = f
      val y = x ++ (after, before)
      y
    }
  }
  
  private def getChildrenIndentation(p: Tree, t: Tree): Option[String] = {
    if (p.hasExistingCode && !(p.isInstanceOf[If] && t.isInstanceOf[Block])/*prevent that we indent too much*/) {
    
     val childrenOnNewLine = children(p) filter (_.pos.isRange) filter (_.pos.line != p.pos.line)
     
     if(childrenOnNewLine exists (_ samePos t)) {
       Some(indentation(t))
     } else     
       childrenOnNewLine.headOption map indentation 
       
    } else 
      None
  }
}