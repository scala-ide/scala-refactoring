package scala.tools.refactoring
package sourcegen

trait AstTransformations {
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  implicit def treesToTraversalFunction(tree: Tree) = (transform: (Tree => Tree)) => tree match {
    case EmptyTree => 
      tree
    // FIXME implement the rest
  }
  
  val transformations = new TreeTransformations[Tree]
  import transformations._

}