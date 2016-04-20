package scala.tools.refactoring
package implementations.oimports

import scala.reflect.internal.util.Position
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TextChange
import scala.util.Properties
import sourcegen.Formatting

case class Region private (imports: List[Global#Import], owner: Global#Symbol, startPos: Global#Position, endPos: Global#Position,
    source: SourceFile, indentation: String, printImport: Global#Import => String, printImportWithComment: Global#Import => String) {
  def transform(transformation: List[Global#Import] => List[Global#Import]): Region =
    copy(imports = transformation(imports))

  private def printEmptyImports: Change = {
    val fromBeginningOfLine = source.lineToOffset(source.offsetToLine(startPos.start))
    val from = if (source.content.slice(fromBeginningOfLine, startPos.start).exists(!_.isWhitespace))
      startPos.start
    else
      fromBeginningOfLine
    val toEndOfLine = endPos.end + Properties.lineSeparator.length
    val lastLineNumber = source.offsetToLine(endPos.end)
    val lastLine = source.lineToString(lastLineNumber)
    val beginningOfLastLine = source.lineToOffset(lastLineNumber)
    val to = if (lastLine.drop(endPos.end - beginningOfLastLine).exists(!_.isWhitespace))
      endPos.end
    else
      toEndOfLine
    TextChange(source, from, to, "")
  }

  def print: Change = if (imports.nonEmpty) printNonEmptyImports else printEmptyImports

  private def printNonEmptyImports: Change = {
    val from = startPos.pos.start
    val to = endPos.pos.end
    val text = imports.zipWithIndex.foldLeft("") { (acc, imp) =>
      def isLast(idx: Int) = idx == imports.size - 1
      imp match {
        case (imp, 0) if isLast(0) =>
          acc + printImportWithComment(imp)
        case (imp, 0) =>
          acc + printImportWithComment(imp) + Properties.lineSeparator
        case (imp, idx) if isLast(idx) =>
          acc + indentation + printImportWithComment(imp)
        case (imp, _) =>
          acc + indentation + printImportWithComment(imp) + Properties.lineSeparator
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

  private def scanForComments[G <: Global](global: G)(source: SourceFile): List[RangePosition] = {
    val scanners = new TreeToolboxScanners(global)
    val commentScanner = new scanners.CommentScanner(source)
    commentScanner.scan()
    commentScanner.comments
  }

  private def cutPrefixSuffix(imp: Global#Import, source: SourceFile): (String, List[String]) = {
    val printedImport = source.content.slice(imp.pos.start, imp.pos.end).mkString
    val prefixPatternWithCommentInside = """import\s+(((\/\*.*\*\/)*((\w|\d|_|-)+|(\`.*\`)+)(\/\*.*\*\/)*)\.)+(\/\*.*\*\/)*""".r
    val prefix = prefixPatternWithCommentInside.findFirstIn(printedImport).get
    def toNameRename(printedSelectors: String): List[String] = {
      val unwrapFromBraces = (if (printedSelectors.startsWith("{"))
        printedSelectors.drop(1).dropRight(1)
      else printedSelectors).split(",").filter { _ != "" }.map { _.trim }
      unwrapFromBraces.toList
    }
    val rawSelectors = printedImport.substring(prefix.length).trim
    (prefix, toNameRename(rawSelectors))
  }

  private def selectorToSuffix(suffices: List[String], sel: Global#ImportSelector): Option[String] = suffices.find { s =>
    val name = sel.name.decoded
    val backtickedName = "`" + name + "`"
    val rename = if (sel.rename != null) sel.rename.decoded else ""
    val backtickedRename = "`" + rename + "`"
    def isCorrect(s: String): Boolean = {
      val renameArrowOrNothingLeft = """(\s*=>\s*)?""".r
      renameArrowOrNothingLeft.findAllIn(s).nonEmpty
    }
    val found = if (s.startsWith(name)) {
      if (s.endsWith(backtickedRename)) {
        isCorrect(s.drop(name.length).dropRight(backtickedRename.length))
      } else if (s.endsWith(rename)) {
        isCorrect(s.drop(name.length).dropRight(rename.length))
      } else false
    } else if (s.startsWith(backtickedName)) {
      if (s.endsWith(backtickedRename)) {
        isCorrect(s.drop(backtickedName.length).dropRight(backtickedRename.length))
      } else if (s.endsWith(rename)) {
        isCorrect(s.drop(backtickedName.length).dropRight(rename.length))
      } else false
    } else false
    found
  }

  private def findUpNeighborComment(impPos: Position, comments: List[RangePosition], source: SourceFile): Option[RangePosition] = impPos match {
    case rangePos: RangePosition =>
      val beginningOfImportLine = source.lineToOffset(source.offsetToLine(rangePos.start))
      comments.find { comment =>
        comment.end == beginningOfImportLine
      }
    case _ => None
  }

  private def findUpNeighborCommentText(impPos: Position, comments: List[RangePosition], source: SourceFile): Option[String] =
    findUpNeighborComment(impPos, comments, source).map { comment =>
      source.content.slice(comment.start, comment.end).mkString
    }

  private def wrapInBraces(selectors: String, rawSelectors: List[Global#ImportSelector], formatting: Formatting): String =
    if (rawSelectors.length > 1 || rawSelectors.exists { sel =>
      sel.rename != null && sel.name.decoded != sel.rename.decoded
    })
      "{" + formatting.spacingAroundMultipleImports + selectors + formatting.spacingAroundMultipleImports + "}"
    else
      selectors

  def apply[G <: Global](global: G)(imports: List[global.Import], owner: global.Symbol, formatting: Formatting): Region = {
    require(imports.nonEmpty, "List of imports must not be empty.")
    val source = imports.head.pos.source
    val comments = scanForComments(global)(source)
    def printImport(imp: Global#Import): String = {
      val (prefix, suffices) = cutPrefixSuffix(imp, source)
      val suffix = imp.selectors.collect {
        case sel => selectorToSuffix(suffices, sel)
      }.filter(_.nonEmpty).map(_.get)
      prefix + wrapInBraces(suffix.mkString(", "), imp.selectors, formatting)
    }
    val indent = indentation(imports.head)
    def printImportWithComment(imp: Global#Import): String = {
      val printedImport = printImport(imp)
      findUpNeighborCommentText(imp.pos, comments, source).map { comment =>
        comment + indent + printedImport
      }.getOrElse(printedImport)
    }
    val regionStartPos = findUpNeighborComment(imports.head.pos, comments, source).getOrElse(imports.head.pos)
    Region(toRegionImports(global)(imports, owner), owner, regionStartPos, imports.last.pos, source, indent, printImport, printImportWithComment)
  }

  private def toRegionImports[G <: Global](g: G)(imports: List[g.Import], owner: g.Symbol) = {
    val ttb = new TreeToolbox[g.type](g)
    imports.map { i => new ttb.RegionImport(owner, i) }
  }
}
