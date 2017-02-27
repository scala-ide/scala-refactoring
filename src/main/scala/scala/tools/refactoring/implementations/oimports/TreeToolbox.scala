package scala.tools.refactoring
package implementations.oimports

import scala.reflect.internal.util.NoSourceFile
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TextChange
import scala.annotation.tailrec

class TreeToolbox[G <: Global](val global: G) {
  import global._
  import scala.collection._

  private val treeComparables = new TreeComparables[global.type](global)

  class TreeCollector[T <: Tree] private (traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) extends Traverser {
    private val collected_ = mutable.ListBuffer.empty[(T, Symbol)]
    def collect(tree: T, owner: Symbol = currentOwner): Unit = {
      collected_ += (tree -> owner)
    }
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

  import scala.reflect.internal.util.RangePosition
  import scala.tools.refactoring.sourcegen.Formatting
  class RegionImport(val owner: Symbol = NoSymbol, proto: Import, val comments: List[RangePosition] = Nil,
    val printTransform: (Formatting, (String, String)) => (String, String) = (formatting, prefixSuffix) => prefixSuffix)(val positions: Seq[Position] = Option(proto.pos).toSeq)
      extends Import(proto.expr, proto.selectors) with ImportPrinter with RegionOwner {
    setPos(proto.pos).setType(proto.tpe).setSymbol(proto.symbol)

    def changePrintTransform(printTransform: (Formatting, (String, String)) => (String, String)) =
      new RegionImport(owner, Import(expr, selectors).setPos(pos).setSymbol(symbol).setType(tpe), comments, printTransform)(positions)

    override def copy(expr: Tree = this.expr, selectors: List[ImportSelector] = this.selectors) =
      new RegionImport(owner, Import(expr, selectors).setPos(pos).setSymbol(symbol).setType(tpe), comments, printTransform)(positions)

    def merge(that: RegionImport): RegionImport =
      new RegionImport(owner, Import(expr, selectors ::: that.selectors).setPos(pos).setSymbol(symbol).setType(tpe), comments, printTransform)(positions ++ that.positions)

    def spawn: List[RegionImport] = selectors.map { sel =>
      new RegionImport(owner, Import(expr, List(sel)).setPos(pos).setSymbol(symbol).setType(tpe), comments, printTransform)(positions)
    }

    def indentation: String = {
      val sourceFile = pos.source
      sourceFile.lineToString(sourceFile.offsetToLine(pos.start)).takeWhile { _.isWhitespace }
    }

    private def findUpNeighborComment(impPos: Position, comments: List[RangePosition], source: SourceFile): Option[RangePosition] = impPos match {
      case rangePos: RangePosition =>
        val beginningOfImportLine = source.lineToOffset(source.offsetToLine(rangePos.start))
        comments.find { comment =>
          comment.end == beginningOfImportLine
        }
      case _ => None
    }

    def positionWithComment: Position =
      findUpNeighborComment(pos, comments, pos.source).map { p =>
        new RangePosition(pos.source, p.start, p.start, pos.end)
      }.getOrElse(pos)

    private def findUpNeighborCommentText(impPos: Position, comments: List[RangePosition], source: SourceFile): Option[String] =
      findUpNeighborComment(impPos, comments, source).map { comment =>
        source.content.slice(comment.start, comment.end).mkString
      }

    private def wrapInBraces(rawSelectors: List[Global#ImportSelector], formatSelectors: => String, formatDefault: => String): String =
      if (rawSelectors.length > 1 || rawSelectors.exists { sel =>
        sel.rename != null && sel.name.decoded != sel.rename.decoded
      })
        formatSelectors
      else
        formatDefault

    private def isImportedInDefiningPkg: Boolean = {
      def toName(sym: Symbol) = sym.name.decoded
      val out = expr.symbol.ownerChain.filter { _.isPackage }.map { toName } == owner.ownerChain.filter { _.isPackage }.map { toName }
      out
    }

    private def isLocallyDefinedIdentifier: Boolean = expr.exists {
      case This(_) =>
        true
      case _ => false
    }

    private def rangeOnly(positions: Seq[Position]): Seq[Position] = positions.filter { _.isRange }

    def printWithComment(formatting: Formatting): Option[String] = {
      val source = pos.source
      def printImport: Option[String] = {
        val (prefices, sufficesSeq) = rangeOnly(positions).map { extractPrefixSuffixFromPositionIfPossible }.unzip
        val prefix = prefices.headOption.map { prefix =>
          if (!isImportedInDefiningPkg && !isLocallyDefinedIdentifier)
            useAbsolutePkgPathIfPossible(prefix)
          else
            prefix
        }
        val selectorToSuffix = findSelectorInSufficesOrMakeIt(sufficesSeq.flatten.toList)(_)
        val suffix = selectors.map {
          selectorToSuffix
        }.collect {
          case Some(suffix) => suffix
        }
        import RegionImport.{ formatPath, formatSelectors }
        prefix.map { prefix =>
          val (formattedPath, formatDefault) =
            printTransform(formatting, (formatPath(formatting, prefix), wrapInBraces(selectors, formatSelectors(formatting, suffix.mkString(", ")), suffix.mkString(", "))))
          formattedPath + formatDefault
        }
      }
      val indent = indentation
      printImport.map { printedImport =>
        findUpNeighborCommentText(pos, comments, source).map { comment =>
          comment + indent + printedImport
        }.getOrElse(printedImport)
      }
    }
  }

  trait ImportPrinter { self: RegionImport =>
    import scala.reflect.internal.Chars
    private def decodedName(name: Name) = {
      val keywords = global.nme.keywords.map { _.decoded }
      def addBackquotes(str: String) = {
        val (ident, op) =
          if (Chars.isIdentifierStart(str.head))
            str.span(Chars.isIdentifierPart)
          else
            ("", str)
        val needsBackticks =
          if (op.isEmpty)
            keywords(name.toTermName.decoded) && name.toTermName.decoded != "_"
          else if (!ident.isEmpty && ident.last != '_')
            true
          else
            !op.tail.forall(Chars.isOperatorPart)
        if (needsBackticks) s"`$str`" else str
      }
      addBackquotes(name.decoded.trim)
    }

    private def printExprFromSymbols: Option[String] = {
      def addDotWhenNonEmptyValue(value: String) =
        if (value.isEmpty) value else "." + value

      def print(expr: Symbol, printed: Option[String]): Option[String] = expr match {
        case sym if sym == null || sym == NoSymbol || sym.isMethod || sym.isAnonymousFunction => None
        case sym if sym.name.toTermName == nme.ROOT || sym.name.toTermName == nme.EMPTY_PACKAGE_NAME => printed
        case sym if sym.name.toTermName != nme.PACKAGE =>
          print(sym.owner, printed.map { decodedName(sym.name) + addDotWhenNonEmptyValue(_) })
        case sym =>
          print(sym.owner, printed)
      }
      print(expr.symbol, Some(""))
    }

    private def printExprDefaultToTreePrint: Option[String] =
      printExprFromSymbols.orElse(Option(expr.toString))

    private def printExprDefaultToOrigText(origText: String): Option[String] =
      printExprFromSymbols.orElse(Option(origText))

    private def endWithDot(path: Option[String]): String = path.map { path =>
      if (path.endsWith(".")) path else path + "."
    }.getOrElse(".")

    private val commentOpen = "/*"
    private val commentClose = "*/"
    private val oneChar = 1
    private def leaveComment(in: String) = {
      import scala.annotation.tailrec
      @tailrec def collectPkgPath(rest: String, acc: Int, extract: String): String = rest match {
        case rest if rest.isEmpty => extract
        case rest if rest.startsWith(commentOpen) => collectPkgPath(rest.drop(commentOpen.length), acc + 1, extract)
        case rest if rest.startsWith(commentClose) => collectPkgPath(rest.drop(commentClose.length), acc - 1, extract)
        case rest if acc > 0 => collectPkgPath(rest.drop(oneChar), acc, extract)
        case rest => collectPkgPath(rest.drop(oneChar), acc, extract + rest.take(oneChar))
      }
      collectPkgPath(in, 0, "")
    }

    def useAbsolutePkgPathIfPossible(sourceFilePath: String): String = {
      val impReg = "import\\s+".r
      val orig = impReg.replaceFirstIn(leaveComment(sourceFilePath), "")
      val importString = impReg.findFirstIn(sourceFilePath).getOrElse(ImportPrefix)
      val fromExpr = endWithDot(printExprDefaultToOrigText(orig))
      if (fromExpr.startsWith(orig))
        sourceFilePath
      else if (fromExpr.endsWith(orig)) {
        val fromExprPrefix = {
          val prefix = fromExpr.substring(0, fromExpr.indexOf(orig))
          if (prefix.endsWith(".")) prefix else prefix + "."
        }
        importString + fromExprPrefix + orig
      } else
        importString + fromExpr
    }

    private val ImportPrefix = "import "

    private def oneLineMultiImport(rawImport: String): String = {
      if (rawImport.startsWith(ImportPrefix))
        rawImport
      else ImportPrefix + rawImport
    }

    def extractPrefixSuffixFromPositionIfPossible(pos: Position): (String, List[String]) =
      extractPrefixSuffix(pos)
        .getOrElse(ImportPrefix + endWithDot(printExprDefaultToTreePrint) -> selectors.map { sel => decodedName(sel.name) })

    private def extractPrefixSuffix(pos: Position): Option[(String, List[String])] = {
      val printedImport = oneLineMultiImport(pos.source.content.slice(pos.start, pos.end).mkString)
      val prefixPatternWithCommentInside = """import\s+(((\/\*.*\*\/)*((\w|\d|_|-)+|(\`.*\`)+)(\/\*.*\*\/)*)\.)+(\/\*.*\*\/)*""".r
      val prefix = prefixPatternWithCommentInside.findFirstIn(printedImport).orElse(Option(printedImport))
      def toNameRename(printedSelectors: String): List[String] = {
        val unwrapFromBracesAndSplit = (if (printedSelectors.startsWith("{"))
          printedSelectors.drop(1).dropRight(1)
        else printedSelectors).split(",").filter { _ != "" }.map { _.trim }
        unwrapFromBracesAndSplit.toList
      }
      prefix.collect {
        case prefix if prefix != ImportPrefix =>
          val rawSelectors = printedImport.substring(prefix.length).trim
          (prefix, toNameRename(rawSelectors))
      }
    }

    private def mkFromSelector(sel: ImportSelector): String = {
      val renameArrow = " => "
      decodedName(sel.name) + {
        if (sel.rename != null && sel.name.decoded != sel.rename.decoded)
          renameArrow + decodedName(sel.rename)
        else
          ""
      }
    }

    private def selectorToSuffix(suffices: List[String], sel: ImportSelector): Option[String] = suffices.find { s =>
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

    def findSelectorInSufficesOrMakeIt(suffices: List[String])(sel: ImportSelector): Option[String] =
      selectorToSuffix(suffices, sel).orElse(Option(mkFromSelector(sel)))
  }

  object RegionImport {
    def unapply(regionImport: RegionImport): Option[(Tree, List[ImportSelector])] =
      Option((regionImport.expr, regionImport.selectors))

    def formatSelectors(formatting: Formatting, selectors: String): String =
      "{" + formatting.spacingAroundMultipleImports + selectors + formatting.spacingAroundMultipleImports + "}"

    def formatPath(formatting: Formatting, importPrefix: String): String = {
      if (formatting.dropScalaPackage) {
        val ScalaDot = "scala."
        val importReg = "import\\s+".r
        importReg.findPrefixOf(importPrefix).map { importKeywordWithSpaces =>
          val importPath = importPrefix.substring(importKeywordWithSpaces.length)
          if (importPath.startsWith(ScalaDot))
            importKeywordWithSpaces + importPath.substring(ScalaDot.length)
          else
            importPrefix
        }.getOrElse(importPrefix)
      } else {
        importPrefix
      }
    }
  }

  case class Region(imports: List[Import], owner: Symbol = NoSymbol, from: Int = -1,
      to: Int = -1, source: SourceFile = NoSourceFile, indentation: String = "",
      formatting: Formatting = new Formatting {}, printAtTheEndOfRegion: String = "") {
    def transform(transformation: List[Import] => List[Import]): Region =
      copy(imports = transformation(imports))

    @tailrec
    private def findNonBlankLine(to: Int): Int = if (to == source.content.length)
      to
    else if (source.content(to) != '\r' && source.content(to) != '\n')
      to
    else
      findNonBlankLine(to + 1)

    private def printEmptyImports: Change = {
      val fromBeginningOfLine = source.lineToOffset(source.offsetToLine(from))
      val from_ = if (source.content.slice(fromBeginningOfLine, from).exists(!_.isWhitespace))
        from
      else
        fromBeginningOfLine
      val lastLineNumber = source.offsetToLine(to)
      val lastLine = source.lineToString(lastLineNumber)
      val beginOfLastLine = source.lineToOffset(lastLineNumber)
      val to_ = if (lastLine.drop(to - beginOfLastLine).exists(!_.isWhitespace))
        to
      else
        findNonBlankLine(to)
      TextChange(source, from_, to_, printAtTheEndOfRegion)
    }

    def rightIntersectImports(rhs: List[Import]): Region = {
      val isScalaLanguageImport = MiscTools.isScalaLanguageImport(global)
      val rightIntersection = imports.collect {
        case rImp @ RegionImport(rexpr, rsels) =>
          rhs.collect {
            case imp @ Import(expr, sels) if treeComparables.isSame(rImp, imp) ||
              rsels.exists { _.name == global.nme.WILDCARD } && (treeComparables.isSameWithSymbols(true)(expr.symbol, rexpr.symbol) || treeComparables.isSameExprByName(true)(expr, rexpr)) ||
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
          case (imp: RegionImport, 0) if isLast(0) =>
            imp.printWithComment(formatting).map { acc + _ }.getOrElse(acc)
          case (imp: RegionImport, 0) =>
            imp.printWithComment(formatting).map { acc + _ + formatting.lineDelimiter }.getOrElse(acc)
          case (imp: RegionImport, idx) if isLast(idx) =>
            imp.printWithComment(formatting).map { acc + indentation + _ }.getOrElse(acc)
          case (imp: RegionImport, _) =>
            imp.printWithComment(formatting).map { acc + indentation + _ + formatting.lineDelimiter }.getOrElse(acc)
        }
      } + printAtTheEndOfRegion
      TextChange(source, from, to, text)
    }
  }
}

trait RegionOwner {
  def owner: Global#Symbol
}
