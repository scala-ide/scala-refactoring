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
      case imports @ h :: _ => Some(Region(imports, global))
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

import scala.reflect.internal.util.SourceFile
case class Region private (imports: List[Global#Import], startPos: Global#Position, endPos: Global#Position, source: SourceFile, indentation: String, printImport: Global#Import => String) {
  def transform(transformation: List[Global#Import] => List[Global#Import]): Region =
    copy(imports = transformation(imports))

  def print: Change = {
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
  def indentation(imp: Global#Import): String = {
    val sourceFile = imp.pos.source
    sourceFile.lineToString(sourceFile.offsetToLine(imp.pos.start)).takeWhile { _.isWhitespace }
  }

  def apply(imports: List[Global#Import], global: Global): Region = {
    val source = imports.head.pos.source
    def printImport(imp: Global#Import): String = {
      import global._
      val prefix = source.content.slice(imp.pos.start, imp.pos.end).mkString.reverse.dropWhile { _ != '.' }.reverse
      val suffix = imp.selectors.map { sel =>
        if (sel.name == sel.rename || sel.name == nme.WILDCARD)
          sel.name
        else
          sel.name + " => " + sel.rename
      }.mkString(if (imp.selectors.size > 1) "{" else "", ", ", if (imp.selectors.size > 1) "}" else "")
      prefix + suffix
    }
    assert(imports.nonEmpty)
    Region(imports, imports.head.pos, imports.last.pos, source, indentation(imports.head), printImport)
  }
}
