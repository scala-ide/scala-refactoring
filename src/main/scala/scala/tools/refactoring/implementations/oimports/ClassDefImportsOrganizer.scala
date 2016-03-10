package scala.tools.refactoring.implementations
package oimports

import scala.tools.nsc.interactive.Global

class ClassDefImportsOrganizer(val global: Global) {
  private def noAnyTwoImportsInSameLine(importsGroup: List[Global#Import]): Boolean =
    importsGroup.size == importsGroup.map { _.pos.line }.distinct.size

  private def importsGroupsFromTree(trees: List[Global#Tree]): List[List[Global#Import]] = {
    val groupedImports = trees.foldLeft(List.empty[List[Global#Import]]) { (acc, tree) => tree match {
      case imp: Global#Import =>
        val lastUpdated = acc.lastOption.map { _ :+ imp }.getOrElse(List(imp))
        acc.take(acc.length - 1) :+ lastUpdated
      case _ => acc :+ List.empty[Global#Import]
    } }.filter { _.nonEmpty }
    groupedImports
  }

  private val util = new TreeToolbox(global)
  import util.forTreesOfKind

  private def forTreesOfTemplates(tree: Global#Tree) = forTreesOfKind[Global#Template](tree) { treeCollector => {
      case t @ util.global.Template(_, _, body) =>
        treeCollector.collected += t
        body.foreach { treeCollector.traverse }
    }
  }

  private def toRegions(groupedImports: List[List[Global#Import]]): List[Region] =
    groupedImports.map {
      case imports @ h :: _ => Some(Region(imports)(global))
      case _ => None
    }.filter {
      _.nonEmpty
    }.map { _.get }

  def transformTreeToRegions(tree: Global#Tree): List[Region] = toRegions(forTreesOfTemplates(tree).flatMap { template =>
    importsGroupsFromTree(template.body).filter {
      noAnyTwoImportsInSameLine
    }
  })
}
