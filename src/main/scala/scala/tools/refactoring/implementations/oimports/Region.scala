package scala.tools.refactoring
package implementations.oimports

import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TextChange
import scala.util.Properties

import sourcegen.Formatting

case class Region private (imports: List[Global#Import], owner: Global#Symbol, from: Int,
    to: Int, source: SourceFile, indentation: String,
    formatting: Formatting, printAtTheEndOfRegion: String) {
  def transform(transformation: List[Global#Import] => List[Global#Import]): Region =
    copy(imports = transformation(imports))

  private def printEmptyImports: Change = {
    val fromBeginningOfLine = source.lineToOffset(source.offsetToLine(from))
    val from_ = if (source.content.slice(fromBeginningOfLine, from).exists(!_.isWhitespace))
      from
    else
      fromBeginningOfLine
    val toEndOfLine = to + Properties.lineSeparator.length
    val lastLineNumber = source.offsetToLine(to)
    val lastLine = source.lineToString(lastLineNumber)
    val beginningOfLastLine = source.lineToOffset(lastLineNumber)
    val to_ = if (lastLine.drop(to - beginningOfLastLine).exists(!_.isWhitespace))
      to
    else
      toEndOfLine
    TextChange(source, from_, to_, printAtTheEndOfRegion)
  }

  def unionImports[G <: Global](ttb: TreeToolbox[G])(importsToAdd: List[ttb.global.Import]): Region = {
    val importsNotFound = importsToAdd.filterNot { impToAdd =>
      imports.exists { imp =>
        ttb.isSame(imp, impToAdd)
      }
    }
    if (importsNotFound.nonEmpty) {
      val groupedImports = imports.filter {
        _.selectors.exists { sel =>
          sel.name == ttb.global.nme.WILDCARD
        }
      }.flatMap {
        case rImp @ ttb.RegionImport(rexpr, _) =>
          importsNotFound.collect {
            case imp @ ttb.global.Import(expr, sels) if ttb.isSameExpr(true)(rexpr.symbol, expr.symbol) =>
              new ttb.RegionImport(rImp.owner, imp.setPos(rImp.pos).setType(rImp.tpe).setSymbol(rImp.symbol))()
          }
      }
      if (groupedImports.nonEmpty)
        copy(imports = this.imports ::: groupedImports)
      else
        this
    } else
      this
  }

  def rightIntersectImports[G <: Global](ttb: TreeToolbox[G])(rhs: List[ttb.global.Import]): Region = {
    val isScalaLanguageImport = MiscTools.isScalaLanguageImport(ttb.global)
    val rightIntersection = imports.collect {
      case rImp @ ttb.RegionImport(rexpr, rsels) =>
        rhs.collect {
          case imp @ ttb.global.Import(expr, sels) if ttb.isSame(rImp, imp) ||
            rsels.exists { _.name == ttb.global.nme.WILDCARD } && (ttb.isSameExpr(true)(expr.symbol, rexpr.symbol) || ttb.isSameExprByName(true)(expr, rexpr)) ||
            isScalaLanguageImport(imp) =>
            rImp.copy(selectors = sels)
        }
    }
    copy(imports = rightIntersection.flatten)
  }

  def print: Change = if (imports.nonEmpty) printNonEmptyImports else printEmptyImports

  private def printNonEmptyImports: Change = {
    val text = imports.zipWithIndex.foldLeft("") { (acc, imp) =>
      def isLast(idx: Int) = idx == imports.size - 1
      imp match {
        case (imp: TreeToolbox[_]#RegionImport, 0) if isLast(0) =>
          acc + imp.printWithComment(formatting)
        case (imp: TreeToolbox[_]#RegionImport, 0) =>
          acc + imp.printWithComment(formatting) + Properties.lineSeparator
        case (imp: TreeToolbox[_]#RegionImport, idx) if isLast(idx) =>
          acc + indentation + imp.printWithComment(formatting)
        case (imp: TreeToolbox[_]#RegionImport, _) =>
          acc + indentation + imp.printWithComment(formatting) + Properties.lineSeparator
      }
    } + printAtTheEndOfRegion
    TextChange(source, from, to, text)
  }
}

object Region {
  private def scanForComments[G <: Global](global: G)(source: SourceFile): List[RangePosition] = {
    val scanners = new TreeToolboxScanners(global)
    val commentScanner = new scanners.CommentScanner(source)
    commentScanner.scan()
    commentScanner.comments
  }

  def apply[G <: Global, T <: TreeToolbox[G]](treeToolbox: T)(imports: List[treeToolbox.global.Import], owner: treeToolbox.global.Symbol, formatting: Formatting, printAtEndOfRegion: String = ""): Region = {
    require(imports.nonEmpty, "List of imports must not be empty.")
    val source = imports.head.pos.source
    val comments = scanForComments(treeToolbox.global)(source)
    val regionImports = toRegionImports[G, T](treeToolbox)(imports, owner, comments)
    val regionStartPos = regionImports.head.positionWithComment.start
    val indentation = regionImports.head.indentation
    Region(regionImports, owner, regionStartPos, imports.last.pos.end, source, indentation, formatting, printAtEndOfRegion)
  }

  private def toRegionImports[G <: Global, T <: TreeToolbox[G]](ttb: T)(imports: List[ttb.global.Import], owner: ttb.global.Symbol, comments: List[RangePosition]) = {
    imports.map { i => new ttb.RegionImport(owner, i, comments)() }
  }
}
