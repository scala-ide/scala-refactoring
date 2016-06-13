package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.Global
import scala.annotation.tailrec

class TreeToolbox[G <: Global](val global: G) {
  import global._
  import scala.collection._

  class TreeCollector[T <: Tree] private (traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) extends Traverser {
    private val collected_ = mutable.ListBuffer.empty[(T, Symbol)]
    def collect(tree: T, owner: Symbol = currentOwner): Unit = collected_ += (tree -> owner)
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

  @tailrec private def skipPackageClassSymbol(sym: Global#Symbol): Global#Symbol = sym match {
    case sym if sym.isPackageObjectClass =>
      skipPackageClassSymbol(sym.owner)
    case sym => sym
  }

  def isSameExpr(acc: Boolean)(leftOwner: Global#Symbol, rightOwner: Global#Symbol): Boolean = {
    val left = Option(skipPackageClassSymbol(leftOwner)).getOrElse(NoSymbol)
    val right = Option(skipPackageClassSymbol(rightOwner)).getOrElse(NoSymbol)
    if (left == NoSymbol && right == NoSymbol)
      acc
    else
      isSameExpr(acc && left.decodedName == right.decodedName)(left.owner, right.owner)
  }

  def isSame(left: Global#Import, right: Global#Import): Boolean = {
    def toNames(imp: Global#Import) = imp.selectors.map { _.name.decoded }.toSet
    isSameExpr(true)(left.expr.symbol, right.expr.symbol) && (toNames(left) & toNames(right)).nonEmpty
  }

  def isSameExprByName(acc: Boolean)(left: Global#Tree, right: Global#Tree): Boolean = (left, right) match {
    case (Select(lqual: Select, lname), Select(rqual: Select, rname)) =>
      isSameExprByName(lname.decoded == rname.decoded && acc)(lqual, rqual)
    case (Select(Ident(lqname), lname), Select(Ident(rqname), rname)) =>
      acc && lqname.decoded == rqname.decoded && lname.decoded == rname.decoded
    case _ => false
  }

  def printExpr(imp: Import): Option[String] = {
    import global.nme
    val keywords = nme.keywords.map { _.decoded }
    def print(expr: Symbol, printed: Option[String]): Option[String] = expr match {
      case sym if sym == null || sym == NoSymbol || sym.isMethod || sym.isAnonymousFunction => None
      case sym if sym.name.toTermName == nme.ROOT || sym.name.toTermName == nme.EMPTY_PACKAGE_NAME => printed
      case sym if sym.name.toTermName != nme.PACKAGE =>
        print(sym.owner, printed.map { decodedName(keywords)(sym.name) + "." + _ })
      case sym =>
        print(sym.owner, printed)
    }
    print(imp.expr.symbol, Some(""))
  }

  import scala.reflect.internal.Chars
  private def decodedName(keywords: Set[String])(name: Global#Name) = {
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

  object removeScopesDuplicates {
    private def isAncestorOf(kid: Region, elder: Region): Boolean = {
      val kidOwner = kid.owner
      val elderOwner = elder.owner
      kidOwner.ownerChain.contains(elderOwner)
    }

    def apply(regions: List[Region]): List[Region] = {
      regions.sortBy {
        _.from
      }.map { kid =>
        val ancestors = regions.filter { potentialAncestor =>
          potentialAncestor.from < kid.from && isAncestorOf(kid, potentialAncestor)
        }
        val ancestorsImports = ancestors.flatMap { _.imports }
        kid.copy(imports = kid.imports.collect {
          case imp if ancestorsImports.find { ancestor => isSame(imp, ancestor) }.isEmpty => imp
        })
      }
    }
  }

  import scala.reflect.internal.util.RangePosition
  import scala.tools.refactoring.sourcegen.Formatting
  class RegionImport(val owner: Symbol, proto: Import, val comments: List[RangePosition] = Nil)(val positions: Seq[Position] = Option(proto.pos).toSeq) extends Import(proto.expr, proto.selectors) with RegionOwner {
    setPos(proto.pos).setType(proto.tpe).setSymbol(proto.symbol)

    override def copy(expr: Tree = this.expr, selectors: List[ImportSelector] = this.selectors) =
      new RegionImport(owner, Import(expr, selectors).setPos(pos).setSymbol(symbol).setType(tpe), comments)(positions)

    def merge(that: RegionImport): RegionImport =
      new RegionImport(owner, Import(expr, selectors ::: that.selectors).setPos(pos).setSymbol(symbol).setType(tpe), comments)(positions ++ that.positions)

    def spawn: List[RegionImport] = selectors.map { sel =>
      new RegionImport(owner, Import(expr, List(sel)).setPos(pos).setSymbol(symbol).setType(tpe), comments)(positions)
    }

    def indentation: String = {
      val sourceFile = pos.source
      sourceFile.lineToString(sourceFile.offsetToLine(pos.start)).takeWhile { _.isWhitespace }
    }

    import scala.reflect.internal.util.SourceFile

    private def mkFromSelector(keywords: Set[String])(sel: ImportSelector): String = {
      val renameArrow = " => "
      decodedName(keywords)(sel.name) + {
        if (sel.rename != null && sel.name.decoded != sel.rename.decoded)
          renameArrow + decodedName(keywords)(sel.rename)
        else
          ""
      }
    }

    private def oneLineMultiImport(rawImport: String): String = {
      val Import = "import "
      if (rawImport.startsWith(Import))
        rawImport
      else Import + rawImport
    }

    private def cutPrefixSuffix(pos: Position): Option[(String, List[String])] = {
      val printedImport = oneLineMultiImport(pos.source.content.slice(pos.start, pos.end).mkString)
      val prefixPatternWithCommentInside = """import\s+(((\/\*.*\*\/)*((\w|\d|_|-)+|(\`.*\`)+)(\/\*.*\*\/)*)\.)+(\/\*.*\*\/)*""".r
      val prefix = prefixPatternWithCommentInside.findFirstIn(printedImport)
      def toNameRename(printedSelectors: String): List[String] = {
        val unwrapFromBraces = (if (printedSelectors.startsWith("{"))
          printedSelectors.drop(1).dropRight(1)
        else printedSelectors).split(",").filter { _ != "" }.map { _.trim }
        unwrapFromBraces.toList
      }
      prefix.map { prefix =>
        val rawSelectors = printedImport.substring(prefix.length).trim
        (prefix, toNameRename(rawSelectors))
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
      expr.symbol.ownerChain.filter { _.isPackage }.map { toName } == owner.ownerChain.filter { _.isPackage }.map { toName }
    }

    def printWithComment(formatting: Formatting): String = {
      import global.nme
      import ImportPrintingStratagems._
      val source = pos.source
      def printImport: String = {
        val (prefices, sufficesSeq) = positions.map { position => cutPrefixSuffix(position).getOrElse("import " + expr.toString + "." -> selectors.map { _.name.decoded }) }.unzip
        val prefix = if (!isImportedInDefiningPkg) useAbsolutePkgPathIfPossible(prefices.head, printExpr(this)) else prefices.head
        val suffices = sufficesSeq.flatten.toList
        val keywords = nme.keywords.map { _.decoded }
        val suffix = selectors.collect {
          case sel => selectorToSuffix(suffices, sel).orElse(Option(mkFromSelector(keywords)(sel)))
        }.filter(_.nonEmpty).map(_.get)
        import RegionImport.{ formatPath, formatSelectors }
        val formatDefault = suffix.mkString(", ")
        formatPath(formatting, prefix) +
          wrapInBraces(selectors, formatSelectors(formatting, formatDefault), formatDefault)
      }
      val indent = indentation
      val printedImport = printImport
      findUpNeighborCommentText(pos, comments, source).map { comment =>
        comment + indent + printedImport
      }.getOrElse(printedImport)
    }
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

  object ImportPrintingStratagems {
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

    def useAbsolutePkgPathIfPossible(sourceFilePath: String, importPath: Option[String]): String = {
      val impReg = "import\\s+".r
      val orig = impReg.replaceFirstIn(leaveComment(sourceFilePath), "")
      val importString = impReg.findFirstIn(sourceFilePath).getOrElse("import ")
      val fromExpr = importPath.getOrElse(orig)
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
  }
}

object MiscTools {
  // Always add the SIP 18 language imports as required until we can handle them properly
  def isScalaLanguageImport[G <: Global](g: G): g.Import => Boolean = {
    import g._
    val language = newTermName("language")
    def apply(candidate: Import) = candidate match {
      case Import(select @ Select(Ident(nme.scala_), `language`), feature) => true
      case _ => false
    }
    apply
  }
}

trait RegionOwner {
  def owner: Global#Symbol
}
