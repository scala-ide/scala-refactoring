package scala.tools.refactoring
package implementations.oimports

import scala.annotation.tailrec
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global

import sourcegen.Formatting

object RegionBuilder {
  private def scanForComments[G <: Global](global: G)(source: SourceFile): List[RangePosition] = {
    val scanners = new TreeToolboxScanners(global)
    val commentScanner = new scanners.CommentScanner(source)
    commentScanner.scan()
    commentScanner.comments
  }

  def apply[G <: Global, T <: TreeToolbox[G]](treeToolbox: T)(imports: List[treeToolbox.global.Import], owner: treeToolbox.global.Symbol, formatting: Formatting, printAtEndOfRegion: String = ""): treeToolbox.Region = {
    require(imports.nonEmpty, "List of imports must not be empty.")
    val source = imports.head.pos.source
    val comments = scanForComments(treeToolbox.global)(source)
    val regionImports = toRegionImports[G, T](treeToolbox)(imports, owner, comments)
    val regionStartPos = regionImports.head.positionWithComment.start
    val indentation = regionImports.head.indentation
    treeToolbox.Region(regionImports, owner, regionStartPos, imports.last.pos.end, source, indentation, formatting, printAtEndOfRegion)
  }

  private def toRegionImports[G <: Global, T <: TreeToolbox[G]](ttb: T)(imports: List[ttb.global.Import], owner: ttb.global.Symbol, comments: List[RangePosition]) = {
    imports.map { i => new ttb.RegionImport(owner, i, comments)() }
  }
}

class TreeComparables[G <: Global](val global: G) {
  import global._
  @tailrec private def skipPackageClassSymbol(sym: Symbol): Symbol = sym match {
    case sym if sym.isPackageObjectClass =>
      skipPackageClassSymbol(sym.owner)
    case sym => sym
  }

  private def owner(sym: Symbol) =
    if (sym != NoSymbol) sym.owner else NoSymbol

  @tailrec final def isSameWithSymbols(acc: Boolean)(leftOwner: Symbol, rightOwner: Symbol): Boolean = {
    val left = Option(skipPackageClassSymbol(leftOwner)).getOrElse(NoSymbol)
    val right = Option(skipPackageClassSymbol(rightOwner)).getOrElse(NoSymbol)
    if (left == NoSymbol && right == NoSymbol)
      acc
    else
      isSameWithSymbols(acc && left.decodedName == right.decodedName)(owner(left), owner(right))
  }

  def isSame(left: Import, right: Import): Boolean = {
    def toNames(imp: Import) = imp.selectors.map { _.name.decoded }.toSet
    isSameWithSymbols(true)(left.expr.symbol, right.expr.symbol) && (toNames(left) & toNames(right)).nonEmpty
  }

  @tailrec final def isSameExprByName(acc: Boolean)(left: Tree, right: Tree): Boolean = (left, right) match {
    case (Select(lqual: Select, lname), Select(rqual: Select, rname)) =>
      isSameExprByName(lname.decoded == rname.decoded && acc)(lqual, rqual)
    case (Select(Ident(lqname), lname), Select(Ident(rqname), rname)) =>
      acc && lqname.decoded == rqname.decoded && lname.decoded == rname.decoded
    case _ => false
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
