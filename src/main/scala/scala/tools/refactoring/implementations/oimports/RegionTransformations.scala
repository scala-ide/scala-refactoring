package scala.tools.refactoring.implementations
package oimports

import scala.annotation.tailrec
import scala.util.Properties
import scala.tools.refactoring.sourcegen.Formatting
import scala.reflect.internal.util.RangePosition

class RegionTransformations[O <: OrganizeImports](val oi: O) {
  import oi._
  import oi.global._

  case class GroupImports(groups: List[String]) {
    import OrganizeImports.Algos
    private def nextPositionInitiator(region: Region) = {
      var index = region.from
      () => {
        require(index <= region.to, "highly unlikely when there is more regions than characters in imports")
        val newPos = index
        index += 1
        newPos
      }
    }

    def apply(region: Region): List[Region] = {
      val nextPosition = nextPositionInitiator(region)
      def separatorRegion = {
        val pos = nextPosition()
        region.copy(imports = Nil, from = pos, to = pos + 1, printAtTheEndOfRegion = Properties.lineSeparator + Properties.lineSeparator + region.indentation)
      }
      def copyRegionWithNewPosition(regionToCopy: Int => Region) = {
        val pos = nextPosition()
        regionToCopy(pos)
      }
      def getImportExpression(imp: Import) = imp.expr.toString
      @tailrec def toRegions(imps: List[List[Import]], accu: List[Region]): List[Region] = imps match {
        case Nil =>
          accu
        case imp :: Nil =>
          toRegions(Nil, accu :+ copyRegionWithNewPosition { pos => region.copy(imports = imp, from = pos, to = region.to) })
        case imp :: imps =>
          val groupedRegions = accu :+ copyRegionWithNewPosition { pos => region.copy(imports = imp, from = pos, to = pos + 1) }
          toRegions(imps, groupedRegions :+ separatorRegion)
      }
      val allImports =
        Algos.groupImports(getImportExpression)(groups, region.imports.asInstanceOf[List[Import]]).toList
      allImports match {
        case Nil => List(region.copy(imports = Nil, to = region.to + Properties.lineSeparator.length, printAtTheEndOfRegion = ""))
        case imps :: Nil => List(region)
        case imps => toRegions(imps, Nil)
      }
    }
  }

  class addExpandedImports(selection: Selection) {
    def apply(ttb: TreeToolbox[global.type])(regions: List[Region]): List[Region] = {
      val enclosingPackage = selection.root match {
        case root: PackageDef =>
          val rootPackage = topPackageDef(root)
          ancestorSymbols(rootPackage).map(_.nameString).mkString(".")
        case _ => ""
      }
      val importsToAddWithNoPosition = mkImportTrees(neededImports(selection.root), enclosingPackage)
      regions.map { region =>
        region.rightIntersectImports(ttb)(importsToAddWithNoPosition)
      }
    }
  }

  class recomputeAndModifyUnused(selection: Selection) {
    private val importsNames = neededImports(selection.root).map { importAsString }

    // If parts of the expr aren't ranges, then we have an import that depends on an
    // other import (see OrganizeImportsRecomputeAndModifyTest#importDependingOnImport)
    private def exprIsAllRangePos(expr: Tree) = {
      // no Tree#forall, so we use double-negative
      !expr.exists(t => !t.pos.isRange)
    }

    private def invisiblePartIsDefaultImported(expr: Tree) = {
      findDeepestNeededSelect(expr) exists isQualifierDefaultImported
    }

    private def importAsString(t: Tree): String = {
      ancestorSymbols(t) match {
        case syms if syms.nonEmpty =>
          syms.map(_.nameString).filterNot(_ == "package").mkString(".")
        case Nil =>
          // Imports without symbols, like Scala feature flags, aka "import scala.language.featureX",
          // have no symbol and are handled by the code blow:
          t match {
            case Select(q, n) => importAsString(q) + "." + n
            case _ =>
              logError("Unexpected tree", new AssertionError(s"Tree without symbol that is not a select: $t"))
              ""
          }
      }
    }

    def apply(ttb: TreeToolbox[global.type])(regions: List[Region]): List[Region] = {
      regions.map { region =>
        val filteredImports = region.imports.filter {
          case rImp @ ttb.RegionImport(expr, selectors) =>
            val pkgName = importAsString(expr) + "."
            val neededSelectors = selectors.filter { selector =>
              selector.name == nme.WILDCARD || importsNames.contains(pkgName + selector.name)
            }
            neededSelectors.size == selectors.size && (exprIsAllRangePos(expr) || invisiblePartIsDefaultImported(expr))
        }
        region.copy(imports = filteredImports)
      }
    }
  }

  type ExprString = String
  type ImportSelectorString = String
  class addNewImports(newImports: List[(ExprString, ImportSelectorString)]) {
    private def findTopLeastPackage(tree: Tree)(implicit ttb: TreeToolbox[global.type]): (PackageDef, Symbol) =
      ttb.forTreesOfKind[PackageDef](tree) { treeCollector =>
        {
          case p @ PackageDef(pid, stats) if stats.exists { tree =>
            tree.symbol != null && tree.symbol != NoSymbol && !tree.symbol.isPackage
          } =>
            treeCollector.collect(p)
          case p @ PackageDef(pid, stats) =>
            treeCollector.collect(p)
            stats.foreach { treeCollector.traverse }
        }
      }.last

    private def isTopLeastPackageRegion(topLeastPackage: PackageDef)(region: Region): Boolean =
      region.owner.ownerChain.contains(topLeastPackage.symbol)

    private def mkRegion(ttb: TreeToolbox[global.type], topLeastPackage: PackageDef, formatting: Formatting): Region = {
      val pos = if (topLeastPackage.stats.isEmpty)
        topLeastPackage.pos
      else
        topLeastPackage.stats.head.pos
      val line = pos.source.offsetToLine(pos.start)
      val topNonPkgIndent = {
        val text = pos.source.lineToString(line)
        text.takeWhile { _.isWhitespace }
      }
      val start = pos.start
      val topLeastPkgPos = new RangePosition(pos.source, start, start, start)
      val imports = newImports.map {
        case (qualifier, name) =>
          val imp = mkImportFromStrings(qualifier, name)
          imp.setPos(topLeastPkgPos)
      }
      Region[global.type, TreeToolbox[global.type]](ttb)(imports, topLeastPackage.symbol, formatting, Properties.lineSeparator + Properties.lineSeparator + topNonPkgIndent)
    }

    def apply(ttb: TreeToolbox[global.type])(regions: List[Region], selection: Selection, formatting: Formatting) = {
      val (topLeastPackage, _) = findTopLeastPackage(selection.root)(ttb)
      val containsCandidate = regions.exists { isTopLeastPackageRegion(topLeastPackage) }
      if (containsCandidate) {
        regions.collect {
          case region if isTopLeastPackageRegion(topLeastPackage)(region) =>
            val firstImportInRegionPos = region.imports.head.pos
            val firstImportPosWithoutPkg = new RangePosition(firstImportInRegionPos.source, firstImportInRegionPos.start, firstImportInRegionPos.start, firstImportInRegionPos.start + "import ".length)
            region.copy(imports = region.imports ::: newImports.map {
              case (qualifier, name) =>
                new ttb.RegionImport(region.owner.asInstanceOf[ttb.global.Symbol], mkImportFromStrings(qualifier, name).setPos(firstImportPosWithoutPkg))()
            })
          case region => region
        }
      } else {
        mkRegion(ttb, topLeastPackage, formatting) :: regions
      }
    }
  }
}
