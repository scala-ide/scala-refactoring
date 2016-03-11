package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.Global

class TreeToolbox[G <: Global](val global: G) {
  import scala.collection._
  import global._

  class TreeCollector[T <: Tree] private (traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) extends Traverser {
    val collected = mutable.ListBuffer.empty[T]
    override def traverse(tree: Tree): Unit = traverserBody(this).orElse[Tree, Unit] {
      case t => super.traverse(t)
    }(tree)
  }

  private object TreeCollector {
    def apply[T <: Tree](traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) = new TreeCollector[T](traverserBody)
  }

  def forTreesOfKind[T <: Tree](tree: Tree)(traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]): List[T] = {
    val treeTraverser = TreeCollector[T](traverserBody)
    treeTraverser.traverse(tree)
    treeTraverser.collected.toList
  }
}
