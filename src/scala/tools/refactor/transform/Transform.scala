package scala.tools.refactor.transform

import scala.tools.refactor.Compiler

trait Transform {
  
  self: scala.tools.refactor.Compiler =>
  import compiler._

  def reverseClassParameters = new Transformer {
    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
      case x => x
    }
  }
  
}
