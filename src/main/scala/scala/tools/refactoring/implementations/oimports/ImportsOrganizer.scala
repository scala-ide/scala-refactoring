package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.Global

abstract class ImportsOrganizer[G <: Global, U <: TreeToolbox[G]](val treeToolbox: U) {
  import treeToolbox.global._
  type T <: Tree

  private def noAnyTwoImportsInSameLine(importsGroup: List[Import]): Boolean =
    importsGroup.size == importsGroup.map { _.pos.line }.distinct.size

  private def importsGroupsFromTree(trees: List[Tree]): List[List[Import]] = {
    val groupedImports = trees.foldLeft(List.empty[List[Import]]) { (acc, tree) =>
      tree match {
        case imp: Import =>
          val lastUpdated = acc.lastOption.map { _ :+ imp }.getOrElse(List(imp))
          acc.take(acc.length - 1) :+ lastUpdated
        case _ => acc :+ List.empty[Import]
      }
    }.filter { _.nonEmpty }
    groupedImports
  }

  protected def forTreesOf(tree: Tree): List[(T, Symbol)]

  protected def treeChildren(parent: T): List[Tree]

  private def toRegions(groupedImports: List[List[Import]], importsOwner: Symbol): List[Region] =
    groupedImports.collect {
      case imports @ h :: _ => Region(imports, importsOwner)(treeToolbox.global)
    }

  def transformTreeToRegions(tree: Tree): List[Region] = forTreesOf(tree).flatMap {
    case (extractedTree, treeOwner) =>
      val groupedImports = importsGroupsFromTree(treeChildren(extractedTree)).filter {
        noAnyTwoImportsInSameLine
      }
      toRegions(groupedImports, treeOwner)
  }
}

class DefImportsOrganizer[G <: Global, U <: TreeToolbox[G]](override val treeToolbox: U) extends ImportsOrganizer[G, U](treeToolbox) {
  import treeToolbox.global._
  type T = Block
  import treeToolbox.forTreesOfKind

  override protected def forTreesOf(tree: Tree) = forTreesOfKind[Block](tree) { treeCollector =>
    {
      case b @ Block(stats, expr) if treeCollector.currentOwner.isMethod && !treeCollector.currentOwner.isLazy =>
        treeCollector.collect(b)
        stats.foreach { treeCollector.traverse }
        treeCollector.traverse(expr)
    }
  }

  override protected def treeChildren(block: Block) = block.stats
}

class ClassDefImportsOrganizer[G <: Global, U <: TreeToolbox[G]](override val treeToolbox: U) extends ImportsOrganizer[G, U](treeToolbox) {
  import treeToolbox.global._
  type T = Template
  import treeToolbox.forTreesOfKind

  override protected def forTreesOf(tree: Tree) = forTreesOfKind[Template](tree) { treeCollector =>
    {
      case t @ Template(_, _, body) =>
        treeCollector.collect(t)
        body.foreach { treeCollector.traverse }
    }
  }

  override protected def treeChildren(template: Template) = template.body
}
