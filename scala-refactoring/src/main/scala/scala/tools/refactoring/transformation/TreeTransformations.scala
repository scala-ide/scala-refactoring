/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

import tools.nsc.io.AbstractFile

trait TreeTransformations extends Transformations {
  
  this: common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
        
  implicit def treesToTraversalFunction(tree: Tree): (Tree => Tree) => Tree = f => {
    
    /**
     * Hooks into the Scala compiler's Transformer but applies only 
     * one transformation and then returns that result.
     * */
    class TransformOnce extends Transformer {
      
      /**
       * Transforms the children of the trees using `f` and creates
       * a new t with the transformed children
       * */
      def once(t: Tree) = t match {
        
        case t: ImportSelectorTree =>
          t
        
        case t: TypeTree if t.original != null =>
          val typeTree = super.transform(t).asInstanceOf[TypeTree]
          typeTree setOriginal f(t.original)
        
        case t: UnApply =>
          // super does not transform t.fun
          treeCopy.UnApply(tree, transform(t.fun), transformTrees(t.args))
          
        case t => super.transform(t)
      }
      override def transform(t: Tree) = f(t)
    }
    
    (new TransformOnce).once(tree)
  }
  
  def transform(f: PartialFunction[Tree, Tree]) = transformation(f)
  
  def filter(f: PartialFunction[Tree, Boolean]) = predicate(f)
  
  def replaceTree(from: Tree, to: Tree) = ↓(matchingChildren(predicate((t: Tree) => t == from) &> constant(to)))
      
  implicit def replacesTree(t1: Tree) = new {
    def replaces(t2: Tree) = t1 setPos t2.pos
  }
    
  implicit def abstractFileToTree(file: AbstractFile): global.Tree = global.unitOfFile(file).body
  
  /**
   * Replace the first sequence of elements with another sequence.
   * */
  implicit def additionalListMethods[T](l: List[T]) = new {
    def replaceSequence(what: List[T], replacement: List[T]): List[T] = {
      def inner(from: List[T], what: List[T], replacement: List[T]): List[T] = (from, what) match {
        case (Nil, _) => Nil
        case (xs, Nil) => xs
        case (x :: xs, y :: ys) if x == y => replacement ::: inner(xs, ys, Nil)
        case (x :: xs, _) => x :: inner(xs, what, replacement)
      }
      inner(l, what, replacement)
    }
  }
  
  val removeAuxiliaryTrees = ↓(transform {
    
    /* An empty RHS that is implemented as '.. { }' creates a Literal 
     * tree with a range length of 1, remove that tree.
     * */
    case t: Literal if t.pos.isRange && t.pos.end - t.pos.start == 1 && t.toString == "()" => EmptyTree
      
    // hide the implicit "apply" call
    case t @ Select(qualifier: Select, name) if name.toString == "apply" && t.samePos(qualifier) => qualifier
    
    case t: Select if t.name.toString == "<init>" => t.qualifier
    
    case t: Tree if (t.pos == NoPosition || t.pos.isRange) => t
    
    case t: ValDef => emptyValDef
    
    case _ => EmptyTree
  })
  
  object NoBlock {
    def unapply(t: Tree) = t match {
      case _: Block => None
      case _ => Some(t)
    }
  }
}