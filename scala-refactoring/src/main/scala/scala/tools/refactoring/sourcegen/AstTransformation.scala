package scala.tools.refactoring
package sourcegen

trait AstTransformations {
  
  self: common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
    
  implicit def treesToTraversalFunction(tree: Tree): (Tree => Tree) => Tree = f => {
    
    class TransformOnce extends Transformer {
      def once(t: Tree) = t match {
        
        case t: ModifierTree => t.copyAttrs(t)
        
        case t => super.transform(t)
      }
      override def transform(t: Tree) = f(t)
    }
    
    (new TransformOnce).once(tree)
  }
  
  val treetransformations = new TreeTransformations[Tree]
  import treetransformations._

  val keepTree = filter {
    case t: Tree => t.pos == NoPosition || t.pos.isRange
  }
  
  val noPosition = transform[Tree, Tree] {
    case t: Tree => t.pos = NoPosition; t
  }
  
  val emptyTree = transform[Tree, Tree] {
    // FIXME also need other empty trees, like emptyValDef
    case _ => EmptyTree
  }
  
  val removeAuxiliaryTrees = topdown(keepTree andThen id orElse emptyTree)
  
  val emptyAllPositions = topdown(noPosition)
}