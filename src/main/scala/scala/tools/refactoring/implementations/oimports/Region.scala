package scala.tools.refactoring
package implementations.oimports

import scala.reflect.internal.util.Position
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TextChange
import scala.util.Properties

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

  private def scanForComments[G <: Global](global: G, source: SourceFile): List[RangePosition] = {
    val scanners = new TreeToolboxScanners[global.type](global)
    val commentScanner = new scanners.CommentScanner(source)
    commentScanner.scan()
    commentScanner.comments
  }

  private def cutPrefix(imp: Global#Import, source: SourceFile): String = {
    val printedImport = source.content.slice(imp.pos.start, imp.pos.end).mkString
    val prefixPatternWithCommentInside = """import (((\/\*.*\*\/)*(\w|\d|_|-)+(\/\*.*\*\/)*)\.)+(\/\*.*\*\/)*""".r
    prefixPatternWithCommentInside.findFirstIn(printedImport).get
  }

  private def wrapInBackticks(name: Global#Name): String =
    if (name.containsChar('$')) "`" + name.decoded + "`" else name.decoded

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

  def apply(imports: List[Global#Import], owner: Global#Symbol)(global: Global): Region = {
    require(imports.nonEmpty, "List of imports must not be empty.")
    val source = imports.head.pos.source
    val comments = scanForComments(global, source)
    def printImport(imp: Global#Import): String = {
      import global._
      val RenameArrow = " => "
      val prefix = cutPrefix(imp, source)
      val suffix = imp.selectors.map { sel =>
        if (sel.name == sel.rename || sel.name == nme.WILDCARD)
          wrapInBackticks(sel.name)
        else
          wrapInBackticks(sel.name) + RenameArrow + wrapInBackticks(sel.rename)
      }
      val areBracesNeeded = suffix.size > 1 || suffix.exists { _ contains RenameArrow }
      prefix + suffix.mkString(if (areBracesNeeded) "{" else "", ", ", if (areBracesNeeded) "}" else "")
    }
    val indent = indentation(imports.head)
    def printImportWithComment(imp: Global#Import): String = {
      val printedImport = printImport(imp)
      findUpNeighborCommentText(imp.pos, comments, source).map { comment =>
        comment + indent + printedImport
      }.getOrElse(printedImport)
    }
    val regionStartPos = findUpNeighborComment(imports.head.pos, comments, source).getOrElse(imports.head.pos)
    Region(imports, owner, regionStartPos, imports.last.pos, source, indent, printImport, printImportWithComment)
  }
}
