/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

trait TreeTransformations extends Transformations {
  
  this: common.PimpedTrees with common.CompilerAccess =>
  
  import global._
        
  implicit def treesToTraversalFunction(tree: Tree): (Tree => Tree) => Tree = f => {
    
    /**
     * Hooks into the Scala compiler's Transformer but applies only 
     * one transformation and then returns that result.
     */
    object TransformOnce extends Transformer {
      
      /**
       * Transforms the children of the trees using `f` and creates
       * a new t with the transformed children
       */
      def once(t: Tree) = t match {
        
        case _: ImportSelectorTree | _: SourceLayoutTree =>
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
    
    TransformOnce.once(tree)
  }
  
  def transform(f: PartialFunction[Tree, Tree]) = transformation(f)
  
  def filter(f: PartialFunction[Tree, Boolean]) = predicate(f)
  
  def replaceTree(from: Tree, to: Tree) = ↓(matchingChildren(predicate((t: Tree) => t == from) &> constant(to)))
      
  implicit def replacesTree(t1: Tree) = new {
    def replaces(t2: Tree) = t1 setPos t2.pos
  }
    
  implicit def abstractFileToTree(file: tools.nsc.io.AbstractFile): global.Tree = compilationUnitOfFile(file).get.body
  
  /**
   * Replace the first sequence of elements with another sequence.
   */
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
  
  /**
   * Locates the imports in a PackageDef. If we have nested packages, it will only match in the innermost.
   */
  val locatePackageLevelImports = {
    
    object TopLevelInsertPosition {
      def unapply(t: Tree): Option[PackageDef] = 
        
        t match {
        case p @ PackageDef(_, stats @ (NoPackageDef(_) :: _)) => 
          Some(p)
        case p @ PackageDef(_, stats) if stats.filter(_.isInstanceOf[PackageDef]).size > 1 =>
          Some(p)
        case _ => 
          None
      }
    }
    
    transformation[Tree, (PackageDef, List[Import], List[Tree])] {
      case TopLevelInsertPosition(p @ PackageDef(_, stats)) =>
        val (imports, others) = stats partition (_.isInstanceOf[Import])
        (p, imports map (_.asInstanceOf[Import]), others)
    }
  }
  
  val removeAuxiliaryTrees = ↓(transform {

    case t: Tree if (t.pos == NoPosition || t.pos.isRange) => t
    
    case t: ValDef => emptyValDef
    
    case _ => EmptyTree
  })
  
  def shallowDuplicate(orig: Tree): Tree = new Transformer {
    override val treeCopy = new StrictTreeCopier
    override def transform(tree: Tree) = {
      if (tree eq orig) 
        super.transform(tree)
      else 
        tree
    }
  } transform orig
  
  
  val setNoPosition = transform {
    case t: global.Tree => t.pos = global.NoPosition; t
  }
}
