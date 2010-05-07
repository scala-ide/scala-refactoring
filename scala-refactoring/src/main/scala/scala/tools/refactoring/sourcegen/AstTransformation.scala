package scala.tools.refactoring
package sourcegen

trait AstTransformations {
  
  self: common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  val t = Transformations.transform[Tree, Tree] _
    
  implicit def treesToTraversalFunction(tree: Tree): (Tree => Tree) => Tree = f => {
    
    class TransformOnce extends Transformer {
      def once(t: Tree) = t match {
        
        case t: TypeTree if t.original != null =>
          val typeTree = super.transform(t).asInstanceOf[TypeTree]
          typeTree setOriginal f(t.original)
        
        case t => super.transform(t)
      }
      override def transform(t: Tree) = f(t)
    }
    
    (new TransformOnce).once(tree)
  }
  
  import Transformations._
  
  val removeUnneededTrees = transform[Tree, Tree] {
    
    /* An empty RHS that is implemented as '.. { }' creates a Literal 
     * tree with a range length of 1, remove that tree.
     * */
    case t: Literal if t.pos.isRange && t.pos.end - t.pos.start == 1 && t.toString == "()" => EmptyTree
      
    // hide the implicit "apply" call
    case t @ Select(qualifier: Select, name) if name.toString == "apply" && t.samePos(qualifier) => qualifier
    
      
    case t: Tree if (t.pos == NoPosition || t.pos.isRange) => t
    
    case t: ValDef => emptyValDef
    
    case _ => EmptyTree
  }
  
  val noPosition = t {
    case t: Tree => t.pos = NoPosition; t
  }
  
  val emptyTree = t {
    case t: ValDef => emptyValDef
    case _ => EmptyTree
  }
  
  val removeAuxiliaryTrees = ↓(removeUnneededTrees)
  
  val emptyAllPositions = ↓(noPosition)
}