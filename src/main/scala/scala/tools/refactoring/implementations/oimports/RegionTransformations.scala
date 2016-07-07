package scala.tools.refactoring.implementations
package oimports

import scala.annotation.tailrec
import scala.reflect.internal.util.RangePosition
import scala.tools.refactoring.sourcegen.Formatting
import scala.util.Properties

class RegionTransformationsContext[O <: OrganizeImports](val oi: O) {
  import oi._
  class RegionTransformations[T <: TreeToolbox[oi.global.type]](val ttb: T) {
    import ttb._
    import ttb.global._

    private val treeComparables = new TreeComparables[oi.global.type](oi.global)

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
          Algos.groupImports(getImportExpression)(groups, region.imports).toList
        allImports match {
          case Nil => List(region.copy(imports = Nil, to = region.to + Properties.lineSeparator.length, printAtTheEndOfRegion = ""))
          case imps :: Nil => List(region)
          case imps => toRegions(imps, Nil)
        }
      }
    }

    class addExpandedImports(selection: Selection) {
      def apply(regions: List[Region]): List[Region] = {
        val enclosingPackage = selection.root match {
          case root: PackageDef =>
            val rootPackage = topPackageDef(root)
            ancestorSymbols(rootPackage).map(_.nameString).mkString(".")
          case _ => ""
        }
        val importsToAddWithNoPosition = mkImportTrees(neededImports(selection.root), enclosingPackage)
        regions.map { region =>
          region.rightIntersectImports(importsToAddWithNoPosition)
        }
      }
    }

    class recomputeAndModifyUnused(selection: Selection) {
      private val importsNames = neededImports(selection.root).map { importAsString }

      // If parts of the expr aren't ranges, then we have an import that depends on an
      // other import (see OrganizeImportsRecomputeAndModifyTest#importDependingOnImport)
      def exprIsAllRangePos(expr: Tree) = {
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

      private def mkIsInImports(expr: Tree): ImportSelector => Boolean = {
        def isSelectorInImports(pkgName: String)(selector: ImportSelector): Boolean =
          selector.name == nme.WILDCARD || importsNames.contains(pkgName + selector.name)
        val pkgName = importAsString(expr) + "."
        isSelectorInImports(pkgName)
      }

      def apply(regions: List[Region]): List[Region] = {
        regions.map { region =>
          val neededImports = region.imports.filter {
            case rImp @ RegionImport(expr, selectors) =>
              val isInImports = mkIsInImports(expr)
              selectors.exists { isInImports }
          }.collect {
            case rImp @ RegionImport(expr, selectors) if expr.pos.isRange || invisiblePartIsDefaultImported(expr) =>
              val isInImports = mkIsInImports(expr)
              rImp.copy(selectors = selectors.filter { isInImports })
          }
          region.copy(imports = neededImports)
        }
      }
    }

    type ExprString = String
    type ImportSelectorString = String
    class addNewImports(newImports: List[(ExprString, ImportSelectorString)]) {
      private def findTopLeastPackage(tree: Tree): (PackageDef, Symbol) =
        forTreesOfKind[PackageDef](tree) { treeCollector =>
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
        region.owner.ownerChain.contains(topLeastPackage.symbol.asTerm.referenced)

      private def mkRegion(topLeastPackage: PackageDef, formatting: Formatting): Region = {
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
        RegionBuilder[ttb.global.type, ttb.type](ttb)(imports, topLeastPackage.symbol, formatting, Properties.lineSeparator + Properties.lineSeparator + topNonPkgIndent)
      }

      def apply(regions: List[Region], selection: Selection, formatting: Formatting) = {
        val (topLeastPackage, _) = findTopLeastPackage(selection.root)
        val containsCandidate = regions.exists { isTopLeastPackageRegion(topLeastPackage) }
        if (containsCandidate) {
          regions.collect {
            case region if isTopLeastPackageRegion(topLeastPackage)(region) =>
              val firstImportInRegionPos = region.imports.head.pos
              val firstImportPosWithoutPkg = new RangePosition(firstImportInRegionPos.source, firstImportInRegionPos.start, firstImportInRegionPos.start, firstImportInRegionPos.start + "import ".length)
              region.copy(imports = region.imports ::: newImports.map {
                case (qualifier, name) =>
                  new RegionImport(region.owner, mkImportFromStrings(qualifier, name).setPos(firstImportPosWithoutPkg))()
              })
            case region => region
          }
        } else {
          (if (newImports.nonEmpty)
            List(mkRegion(topLeastPackage, formatting))
          else
              Nil) ::: regions
        }
      }
    }

    class collapseToWildcard(maxIndividualImports: Int = 2, exclude: Set[String] = Set()) {
      private def isArtificialImport(expr: Tree): Boolean =
        expr.tpe == null

      private def isApplicable(expr: Tree, sels: List[ImportSelector], types: Seq[Name]): Boolean = {
        val exprTypes = expr.tpe.members.map { _.name }.toSet[Name]
        val exprString = expr.toString
        sels.size > maxIndividualImports &&
          (types.toSet & exprTypes).isEmpty &&
          sels.forall { sel => sel.name == sel.rename } &&
          !exclude.contains(exprString)
      }

      def apply(regions: List[Region]): List[Region] = {
        val foundArtificial = regions.exists { region =>
          region.imports.exists {
            case rImp @ RegionImport(expr, _) => isArtificialImport(expr)
          }
        }
        if (foundArtificial)
          regions
        else {
          import scala.collection.mutable.ListBuffer
          val types = ListBuffer.empty[Name]
          regions.map { region =>
            val rimps = region.imports.iterator.map {
              case imp @ RegionImport(expr, sels) if sels.exists { _.name == nme.WILDCARD } =>
                types ++= expr.tpe.members.map { _.name }.toSet
                imp
              case imp => imp
            }.map {
              case imp @ RegionImport(expr, sels) if isApplicable(expr, sels, types) =>
                types ++= expr.tpe.members.map { _.name }.toSet
                val selectorsToWildcard = (formatting: Formatting, prefixSuffix: (String, String)) => {
                  val (prefix, _) = imp.printTransform(formatting, prefixSuffix)
                  (prefix, "_")
                }
                imp.changePrintTransform(selectorsToWildcard)
              case imp => imp
            }
            region.copy(imports = rimps.toList)
          }
        }
      }
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
            case imp if ancestorsImports.find { ancestor =>
              treeComparables.isSame(imp, ancestor) }.isEmpty => imp
          })
        }
      }
    }
  }
}
