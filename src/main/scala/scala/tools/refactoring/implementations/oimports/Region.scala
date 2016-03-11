package scala.tools.refactoring
package implementations.oimports

import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TextChange
import scala.util.Properties

case class Region private (imports: List[Global#Import], startPos: Global#Position, endPos: Global#Position,
    source: SourceFile, indentation: String, printImport: Global#Import => String) {
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
