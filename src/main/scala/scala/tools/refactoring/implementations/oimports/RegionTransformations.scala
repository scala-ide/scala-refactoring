package scala.tools.refactoring.implementations
package oimports

import scala.annotation.tailrec
import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.analysis.CompilationUnitDependencies
import scala.tools.refactoring.common.EnrichedTrees
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.common.TreeExtractors
import scala.tools.refactoring.common.TreeTraverser
import scala.tools.refactoring.sourcegen.Formatting
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.transformation.TreeTransformations

class RegionTransformationsContext[G <: Global](val global: G) extends CompilationUnitDependencies
    with InteractiveScalaCompiler
    with TreeTraverser
    with EnrichedTrees
    with TreeExtractors
    with TreeFactory
    with TreeTransformations  {
  import global._
  class RegionTransformations[T <: TreeToolbox[global.type]](val ttb: T) {
    import ttb._

    private val treeComparables = new TreeComparables[ttb.global.type](ttb.global)

    case class GroupImports(groups: List[String]) {
      import scala.tools.refactoring.implementations.OrganizeImports.Algos
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
          region.copy(imports = Nil, from = pos, to = pos + 1, printAtTheEndOfRegion = region.formatting.lineDelimiter + region.formatting.lineDelimiter + region.indentation)
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
          case Nil => List(region.copy(imports = Nil, printAtTheEndOfRegion = ""))
          case imps :: Nil => List(region)
          case imps => toRegions(imps, Nil)
        }
      }
    }

    class addExpandedImports(root: Tree) {
      def apply(regions: List[Region]): List[Region] = {
        val enclosingPackage = root match {
          case root: PackageDef =>
            val rootPackage = topPackageDef(root)
            ancestorSymbols(rootPackage).map(_.nameString).mkString(".")
          case _ => ""
        }
        val importsToAddWithNoPosition = mkImportTrees(neededImports(root, newWay = true), enclosingPackage)
        regions.map { region =>
          region.rightIntersectImports(importsToAddWithNoPosition)
        }
      }
    }

    class recomputeAndModifyUnused(root: Tree) {
      private val importsNames = neededImports(root, newWay = true).map { importAsString }

      // If parts of the expr aren't ranges, then we have an import that depends on an
      // other import (see OrganizeImportsRecomputeAndModifyTest#importDependingOnImport)
      def exprIsAllRangePos(expr: Tree) = {
        // no Tree#forall, so we use double-negative
        !expr.exists(t => !t.pos.isRange)
      }

      private def invisiblePartIsDefaultImported(expr: Tree) = {
        findDeepestNeededSelect(expr) exists isQualifierDefaultImported
      }

      private def importAsString(t: Tree): String = t match {
        case Select(Ident(name), n) if n != nme.PACKAGE =>
          name.decoded + "." + n
        case Select(q, n) if n != nme.PACKAGE =>
          importAsString(q) + "." + n
        case Select(q, n) if n == nme.PACKAGE =>
          importAsString(q)
        case _ =>
          ancestorSymbols(t) match {
            case syms @ _ :+ last if !last.isMethod =>
              syms.map(_.nameString).filterNot(_ == "package").mkString(".")
            case _ =>
              logError("Unexpected tree", new AssertionError(s"Tree without symbol that is not a select: $t"))
              ""
          }
      }

      private def checkIfSelectorIsNotARenamedPkgName(pkgName: String, selector: ImportSelector, importName: String): Boolean =
        selector.name != selector.rename && importName.startsWith(pkgName + selector.name.decoded)

      private def checkIfImportNameIsRelativeWithPkgName(pkgName: String, selector: ImportSelector, importName: String): Boolean =
        (pkgName + selector.name.decoded).endsWith(importName)

      private def checkIfImportSelectorSuppressImportName(selector: ImportSelector, importName: String): Boolean =
        importName.endsWith(selector.name.decoded) && selector.rename == nme.WILDCARD

      private def mkIsInImports(expr: Tree): ImportSelector => Boolean = {
        def isSelectorInImports(pkgName: String)(selector: ImportSelector): Boolean =
          selector.name == nme.WILDCARD || importsNames.exists { importName =>
            importName == pkgName + selector.name || importName == pkgName + selector.rename ||
              checkIfSelectorIsNotARenamedPkgName(pkgName, selector, importName) ||
              checkIfImportNameIsRelativeWithPkgName(pkgName, selector, importName) ||
              checkIfImportSelectorSuppressImportName(selector, importName)
          }
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
        RegionBuilder[ttb.global.type, ttb.type](ttb)(imports, topLeastPackage.symbol, formatting, formatting.lineDelimiter + formatting.lineDelimiter + topNonPkgIndent).head
      }

      def apply(regions: List[Region], root: Tree, formatting: Formatting) = {
        val (topLeastPackage, _) = findTopLeastPackage(root)
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
              treeComparables.isSame(imp, ancestor)
            }.isEmpty => imp
          })
        }
      }
    }
  }
}
