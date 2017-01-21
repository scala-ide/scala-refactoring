package scala.tools.refactoring
package implementations.oimports

import scala.annotation.tailrec
import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.Global

import sourcegen.Formatting
import scala.tools.refactoring.util.SourceHelpers

object RegionBuilder {
  private def toImportsWithRange[G <: Global](g: G)(imports: List[g.Import]): List[List[g.Import]] = imports.foldLeft(List.empty[List[g.Import]]) { (z, imp) =>
    (imp match {
      case imp if imp.pos.isRange =>
        if (z.isEmpty)
          List(imp) :: z
        else
          (z.head :+ imp) :: z.tail
      case _ =>
        List() :: z
    }).filter { _.nonEmpty }.reverse
  }

  def apply[G <: Global, T <: TreeToolbox[G]](treeToolbox: T)(imports: List[treeToolbox.global.Import], owner: treeToolbox.global.Symbol, formatting: Formatting, printAtEndOfRegion: String = ""): Seq[treeToolbox.Region] = {
    require(imports.nonEmpty, "List of imports must not be empty.")
    val source = imports.head.pos.source
    val comments = SourceHelpers.findComments(source)
    toImportsWithRange[treeToolbox.global.type](treeToolbox.global)(imports).map { imports =>
      val regionImports = toRegionImports[G, T](treeToolbox)(imports, owner, comments)
      val regionStartPos = regionImports.head.positionWithComment.start
      val indentation = regionImports.head.indentation
      treeToolbox.Region(regionImports, owner, regionStartPos, imports.last.pos.end, source, indentation, formatting, printAtEndOfRegion)
    }
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

    @tailrec
    def isScalaLanguage(qual: Select): Boolean = qual match {
      case Select(Ident(nme.scala_), `language`) =>
        true
      case Select(qual: Select, _) =>
        isScalaLanguage(qual)
      case _ =>
        false
    }

    def apply(candidate: Import) = candidate match {
      case Import(select: Select, feature) if isScalaLanguage(select) => true
      case _ => false
    }
    apply
  }

  def stableIdentifierSymbol[G <: Global](g: G)(selectQualifierSymbol: g.Symbol): Option[g.Symbol] = {
    import g._
    @tailrec
    def apply(selectQualifierSymbol: Symbol): Option[Symbol] =
      if (selectQualifierSymbol == null || selectQualifierSymbol == NoSymbol)
        None
      else if (selectQualifierSymbol.isPackage || selectQualifierSymbol.isModuleOrModuleClass
        || selectQualifierSymbol.isStable)
        Some(selectQualifierSymbol)
      else
        apply(selectQualifierSymbol.owner)
    apply(selectQualifierSymbol)
  }
}
