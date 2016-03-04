package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TextChange
import scala.util.Properties
import scala.annotation.implicitNotFound

class DefImportsOrganizer(val global: Global) {
  import global._

  private def noAnyTwoImportsInSameLine(importsGroup: List[Global#Import]): Boolean =
    importsGroup.size == importsGroup.map { _.pos.line }.distinct.size

  private def importsGroupsFromTree(tree: Global#Tree): List[List[Global#Import]] = {
    val impTraverser = new Traverser {
      var groupedImports = List.empty[List[Import]]
      var group = List.empty[Import]
      override def traverse(tree: Tree) = tree match {
        case imp: Import =>
          group = group :+ imp
        case t =>
          if (group.nonEmpty) {
            groupedImports = groupedImports :+ group
            group = List.empty[Import]
          }
          super.traverse(t)
      }
    }
    impTraverser.traverse(tree.asInstanceOf[Tree])
    impTraverser.groupedImports
  }

  private val util = new Util(global)
  import util.forTreesOfKind

  private def forTreesOfBlocks(tree: Global#Tree) = forTreesOfKind[Global#Block](tree) { (collected, currentOwner) => {
      case b: util.global.Block if currentOwner.isMethod && !currentOwner.isLazy =>
        collected += b
    }
  }

  private def toRegions(groupedImports: List[List[Global#Import]]): List[Region] =
    groupedImports.map {
      case imports @ h :: _ => Some(Region(imports))
      case _ => None
    }.filter {
      _.nonEmpty
    }.map { _.get }

  def transformTreeToRegions(tree: Global#Tree): List[Region] = toRegions(forTreesOfBlocks(tree).flatMap { block =>
    importsGroupsFromTree(block).filter {
      noAnyTwoImportsInSameLine
    }
  })
}

class Util(val global: Global) {
  import global._
  import scala.collection._

  private class TreeCollector[T <: Global#Tree](traverserBody: (mutable.ListBuffer[T], Global#Symbol) => PartialFunction[Tree, Unit]) extends Traverser {
    val collected = mutable.ListBuffer.empty[T]
    override def traverse(tree: Tree): Unit = traverserBody(collected, currentOwner).orElse[Tree, Unit] {
      case t => super.traverse(t)
    }(tree)
  }

  def forTreesOfKind[T <: Global#Tree](tree: Global#Tree)(traverserBody: (mutable.ListBuffer[T], Global#Symbol) => PartialFunction[Tree, Unit]): List[T] = {
    val treeTraverser = new TreeCollector[T](traverserBody)
    treeTraverser.traverse(tree.asInstanceOf[Tree])
    treeTraverser.collected.toList
  }
}

case class Region private (val imports: List[Global#Import], val startPos: Global#Position, val endPos: Global#Position) {
  def transform(transformation: List[Global#Import] => List[Global#Import]): Region =
    copy(imports = transformation(imports))

  def print: Change = {
    val sourceFile = imports.head.pos.source
    val from = startPos.pos.start
    val to = endPos.pos.end
    val text = imports.zipWithIndex.foldLeft("") { (acc, imp) =>
      def indentation(imp: Global#Import): String =
        sourceFile.lineToString(sourceFile.offsetToLine(imp.pos.start)).takeWhile { _.isWhitespace }
      def isLast(idx: Int) = idx == imports.size - 1
      imp match {
        case (imp, 0) if isLast(0) =>
          acc + sourceFile.content.slice(imp.pos.start, imp.pos.end).mkString
        case (imp, 0) =>
          acc + sourceFile.content.slice(imp.pos.start, imp.pos.end).mkString + Properties.lineSeparator
        case (imp, idx) if isLast(idx) =>
          acc + indentation(imp) + sourceFile.content.slice(imp.pos.start, imp.pos.end).mkString
        case (imp, _) =>
          acc + indentation(imp) + sourceFile.content.slice(imp.pos.start, imp.pos.end).mkString + Properties.lineSeparator
      }
    }
    TextChange(sourceFile, from, to, text)
  }
}

object Region {
  def apply(imports: List[Global#Import]): Region = {
    assert(imports.nonEmpty)
    Region(imports, imports.head.pos, imports.last.pos)
  }
}

object ImportPrinters {
  @implicitNotFound("ImportPrinter[${I}] not found in scope")
  trait ImportPrinter[I <: Global#Import] {
    def print(imp: I): List[Change]
  }

  object ImportPrinter {
    def apply[I <: Global#Import : ImportPrinter]: ImportPrinter[I] = implicitly[ImportPrinter[I]]
  }

  object Printer {
    def print[I <: Global#Import : ImportPrinter](imp: I) = ImportPrinter[I].print(imp)
  }
}
