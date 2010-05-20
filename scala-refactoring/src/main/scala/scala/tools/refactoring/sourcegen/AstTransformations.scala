package scala.tools.refactoring
package sourcegen

trait AstTransformations {
  
  this: common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  import Transformations._
  
  val transform = Transformations.transform[Tree, Tree] _
    
  implicit def treesToTraversalFunction(tree: Tree): (Tree => Tree) => Tree = f => {
    
    class TransformOnce extends Transformer {
      def once(t: Tree) = t match {
        
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
  
  
  val removeUnneededTrees = transform {
    
    /* An empty RHS that is implemented as '.. { }' creates a Literal 
     * tree with a range length of 1, remove that tree.
     * */
    case t: Literal if t.pos.isRange && t.pos.end - t.pos.start == 1 && t.toString == "()" => EmptyTree
      
    // hide the implicit "apply" call
    case t @ Select(qualifier: Select, name) if name.toString == "apply" && t.samePos(qualifier) => qualifier
    
    case t: Select if t.name.toString == "<init>" => t.qualifier
    
//    case t: New if t.pos.start > t.pos.point => 
//      val p = t.pos withStart t.pos.point withPoint t.pos.start
//      t setPos p
//      
    case t: Tree if (t.pos == NoPosition || t.pos.isRange) => t
    
    case t: ValDef => emptyValDef
    
    case _ => EmptyTree
  }
  
  val noPosition = transform {
    case t: Tree => t.pos = NoPosition; t
  }
  
  val emptyTree = transform {
    case t: ValDef => emptyValDef
    case _ => EmptyTree
  }
  
  val removeAuxiliaryTrees = ↓(removeUnneededTrees)
  
  val emptyAllPositions = ↓(noPosition)
}