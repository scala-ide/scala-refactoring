package scala.tools.refactoring.implementations
package oimports

import scala.annotation.tailrec
import scala.tools.nsc.Global
import scala.util.Properties

class RegionTransformations[G <: Global](val g: G) {
  import g._
  import OrganizeImports.Algos

  case class GroupImports(groups: List[String]) {
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
        region.copy(imports = Nil, from = pos, to = pos + 1, printWhenEmpty = Properties.lineSeparator + Properties.lineSeparator + region.indentation)
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
        Algos.groupImports(getImportExpression)(groups, region.imports.asInstanceOf[List[g.Import]]).toList
      allImports match {
        case Nil => List.empty
        case imps :: Nil => List(region)
        case imps => toRegions(imps, Nil)
      }
    }
  }
}
