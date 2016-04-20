package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.Global

class TreeToolbox[G <: Global](val global: G) {
  import global._
  import scala.collection._

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

    private def isSame(left: Global#Import, right: Global#Import): Boolean = {
      def isSameExpr(acc: Boolean)(leftOwner: Global#Symbol, rightOwner: Global#Symbol): Boolean = {
        val left = Option(leftOwner).getOrElse(NoSymbol)
        val right = Option(rightOwner).getOrElse(NoSymbol)
        if (left == NoSymbol && right == NoSymbol)
          acc
        else
          isSameExpr(acc && left.nameString == right.nameString)(left.owner, right.owner)
      }
      def toNames(imp: Global#Import) = imp.selectors.map { _.name.decoded }.toSet
      isSameExpr(true)(left.expr.symbol, right.expr.symbol) && (toNames(left) & toNames(right)).nonEmpty
    }

    def apply(regions: List[Region]): List[Region] = {
      regions.sortBy {
        _.from
      }.map { kid =>
        val ancestors = regions.filter { potentialAncestor =>
          potentialAncestor.from < kid.from && isAncestorOf(kid, potentialAncestor)
        }
        val ancestorsImports = ancestors.flatMap { _.imports }
        kid.copy(imports = kid.imports.collect {
          case imp if ancestorsImports.find { ancestor => isSame(imp, ancestor) }.isEmpty => imp
        })
      }
    }
  }

  class RegionImport(val owner: Symbol, proto: Import) extends Import(proto.expr, proto.selectors) with RegionOwner {
    setPos(proto.pos).setType(proto.tpe).setSymbol(proto.symbol)

    override def copy(expr: Tree = proto.expr, selectors: List[ImportSelector] = proto.selectors) =
      new RegionImport(owner, proto.copy(expr, selectors).setPos(proto.pos).setSymbol(proto.symbol).setType(proto.tpe))
  }
}

trait RegionOwner {
  def owner: Global#Symbol
}
