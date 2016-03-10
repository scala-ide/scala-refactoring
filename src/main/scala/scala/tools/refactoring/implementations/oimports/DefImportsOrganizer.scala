package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TextChange
import scala.util.Properties

class DefImportsOrganizer(val global: Global) {

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

  private def forTreesOfBlocks(tree: Global#Tree) = forTreesOfKind[Global#Block](tree) { treeCollector => {
    case b @ util.global.Block(stats, expr) if treeCollector.currentOwner.isMethod && !treeCollector.currentOwner.isLazy =>
      treeCollector.collected += b
      stats.foreach { treeCollector.traverse }
      treeCollector.traverse(expr)
    }
  }

  private def toRegions(groupedImports: List[List[Global#Import]]): List[Region] =
    groupedImports.map {
      case imports @ h :: _ => Some(Region(imports)(global))
      case _ => None
    }.filter {
      _.nonEmpty
    }.map { _.get }

  def transformTreeToRegions(tree: Global#Tree): List[Region] = toRegions(forTreesOfBlocks(tree).flatMap { block =>
    importsGroupsFromTree(block.stats).filter {
      noAnyTwoImportsInSameLine
    }
  })
}

class TreeToolbox(val global: Global) {
  import scala.collection._
  import global._

  class TreeCollector[T <: Global#Tree](traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) extends Traverser {
    val collected = mutable.ListBuffer.empty[T]
    override def traverse(tree: Tree): Unit = traverserBody(this).orElse[Tree, Unit] {
      case t => super.traverse(t)
    }(tree)
  }

  def forTreesOfKind[T <: Global#Tree](tree: Global#Tree)(traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]): List[T] = {
    val treeTraverser = new TreeCollector[T](traverserBody)
    treeTraverser.traverse(tree.asInstanceOf[Tree])
    treeTraverser.collected.toList
  }
}

import scala.reflect.internal.util.SourceFile
case class Region private (imports: List[Global#Import], startPos: Global#Position, endPos: Global#Position,
    source: SourceFile, indentation: String, printImport: Global#Import => String) {
  def transform(transformation: List[Global#Import] => List[Global#Import]): Region =
    copy(imports = transformation(imports))

  private def printEmptyImports: Change = {
    val fromBeginningOfLine = source.lineToOffset(source.offsetToLine(startPos.start))
    val toEndOfLine = endPos.end + Properties.lineSeparator.length
    TextChange(source, fromBeginningOfLine, toEndOfLine, "")
  }

  def print: Change = if (imports.nonEmpty) printNonEmptyImports else printEmptyImports

  private def printNonEmptyImports: Change = {
    val from = startPos.pos.start
    val to = endPos.pos.end
    val text = imports.zipWithIndex.foldLeft("") { (acc, imp) =>
      def isLast(idx: Int) = idx == imports.size - 1
      imp match {
        case (imp, 0) if isLast(0) =>
          acc + printImport(imp)
        case (imp, 0) =>
          acc + printImport(imp) + Properties.lineSeparator
        case (imp, idx) if isLast(idx) =>
          acc + indentation + printImport(imp)
        case (imp, _) =>
          acc + indentation + printImport(imp) + Properties.lineSeparator
      }
    }
    TextChange(source, from, to, text)
  }
}

object Region {
  private def indentation(imp: Global#Import): String = {
    val sourceFile = imp.pos.source
    sourceFile.lineToString(sourceFile.offsetToLine(imp.pos.start)).takeWhile { _.isWhitespace }
  }

  def apply(imports: List[Global#Import])(global: Global): Region = {
    assert(imports.nonEmpty)
    val source = imports.head.pos.source
    def printImport(imp: Global#Import): String = {
      import global._
      val RenameArrow = " => "
      val prefix = source.content.slice(imp.pos.start, imp.pos.end).mkString.reverse.dropWhile { _ != '.' }.reverse
      val suffix = imp.selectors.map { sel =>
        if (sel.name == sel.rename || sel.name == nme.WILDCARD)
          sel.name.toString
        else
          sel.name + RenameArrow + sel.rename
      }
      val areBracesNeeded = suffix.size > 1 || suffix.exists { _ contains RenameArrow }
      prefix + suffix.mkString(if (areBracesNeeded) "{" else "", ", ", if (areBracesNeeded) "}" else "")
    }
    Region(imports, imports.head.pos, imports.last.pos, source, indentation(imports.head), printImport)
  }
}
