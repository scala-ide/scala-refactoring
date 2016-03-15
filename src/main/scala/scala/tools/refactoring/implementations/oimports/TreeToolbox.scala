package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.Global

class TreeToolbox[G <: Global](val global: G) {
  import scala.collection._
  import global._

  class TreeCollector[T <: Tree] private (traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) extends Traverser {
    private val collected_ = mutable.ListBuffer.empty[(T, Symbol)]
    def collect(tree: T): Unit = collected_ += (tree -> currentOwner)
    def collected = collected_.toList
    override def traverse(tree: Tree): Unit = traverserBody(this).orElse[Tree, Unit] {
      case t => super.traverse(t)
    }(tree)
  }

  private object TreeCollector {
    def apply[T <: Tree](traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) = new TreeCollector[T](traverserBody)
  }

  def forTreesOfKind[T <: Tree](tree: Tree)(traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]): List[(T, Symbol)] = {
    val treeTraverser = TreeCollector[T](traverserBody)
    treeTraverser.traverse(tree)
    treeTraverser.collected
  }

  object removeScopesDuplicates {
    private def isAncestorOf(kid: Region, elder: Region): Boolean = {
      val kidOwner = kid.owner
      val elderOwner = elder.owner
      kidOwner.ownerChain.contains(elderOwner)
    }

    def apply(regions: List[Region]): List[Region] = {
      regions.sortBy {
        _.startPos.start
      }.map { kid =>
        val ancestors = regions.filter { potentialAncestor =>
          potentialAncestor.startPos.start < kid.startPos.start && isAncestorOf(kid, potentialAncestor) }
        val ancestorsImports = ancestors.flatMap { ancestor =>
          ancestor.imports.map { ancestor.printImport }
        }
        kid.copy(imports = kid.imports.collect {
          case imp if !ancestorsImports.contains(kid.printImport(imp)) => imp
        })
      }
    }
  }
}
