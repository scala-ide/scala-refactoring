package scala.tools.refactoring.implementations.oimports

import scala.tools.refactoring.common.Change
import scala.tools.refactoring.implementations.OrganizeImports

class OrganizeImportsWorker[O <: OrganizeImports](oi: O) {
  import oi._
  import oi.global._

  private val treeToolbox = new TreeToolbox[oi.global.type](oi.global)
  private val regionContext = new RegionTransformationsContext[oi.type](oi)
  private val transformations = new regionContext.RegionTransformations[treeToolbox.type](treeToolbox)
  private val participants = new NotPackageImportParticipants[oi.type](oi)
  private val defImportsOrganizer = new DefImportsOrganizer[treeToolbox.global.type, treeToolbox.type](treeToolbox)
  private val classDefImportsOrganizer = new ClassDefImportsOrganizer[treeToolbox.global.type, treeToolbox.type](treeToolbox)
  private val packageDefImportsOrganizer = new PackageDefImportsOrganizer[treeToolbox.global.type, treeToolbox.type](treeToolbox)

  private def organizeLocalImports(root: Tree): List[treeToolbox.Region] = {
    val defRegions = defImportsOrganizer.transformTreeToRegions(root, oi).map {
      _.transform {
        scala.Function.chain {
          participants.RemoveDuplicatedByWildcard ::
            new participants.RemoveUnused(root) ::
            RemoveDuplicates ::
            SortImportSelectors ::
            participants.SortImports ::
            Nil
        }
      }
    }

    val classDefRegions = classDefImportsOrganizer.transformTreeToRegions(root, oi).map {
      _.transform {
        scala.Function.chain {
          participants.RemoveDuplicatedByWildcard ::
            new participants.RemoveUnused(root) ::
            RemoveDuplicates ::
            SortImportSelectors ::
            participants.SortImports ::
            Nil
        }
      }
    }
    classDefRegions ::: defRegions
  }

  def organizeLocal(selection: Selection) = {
   val rootTree = abstractFileToTree(selection.file)
    val classDefAndDefRegions = organizeLocalImports(rootTree)
    val removedDuplicates = transformations.removeScopesDuplicates(classDefAndDefRegions)
    val changes = removedDuplicates.map { _.print }

    Change.discardOverlappingChanges(changes).accepted
  }

  def organizeAll(selection: Selection, params: RefactoringParameters) = {
    val rootTree = abstractFileToTree(selection.file)

    val classDefAndDefRegions = if (params.organizeLocalImports) {
      organizeLocalImports(rootTree)
    } else {
      Nil
    }
    val config = params.config.get

    val rawPackageRegions = packageDefImportsOrganizer.transformTreeToRegions(rootTree, oi)

    val packageRegions = params.deps match {
      case Dependencies.FullyRecompute =>
        new transformations.addExpandedImports(selection)(rawPackageRegions)
      case Dependencies.RecomputeAndModify =>
        new transformations.recomputeAndModifyUnused(selection)(rawPackageRegions)
      case Dependencies.RemoveUnneeded =>
        new transformations.addNewImports(params.importsToAdd)(rawPackageRegions, selection, oi)
      case _ => rawPackageRegions
    }

    val groupedPackageRegions = if (config.groups.nonEmpty) {
      val groupImport = transformations.GroupImports(config.groups)
      packageRegions.flatMap { groupImport(_) }
    } else packageRegions

    import OrganizeImports.ImportsStrategy
    val expandOrCollapse = config.importsStrategy.map {
      case ImportsStrategy.ExpandImports | ImportsStrategy.PreserveWildcards =>
        new participants.ExpandImports[treeToolbox.type](treeToolbox) :: Nil
      case ImportsStrategy.CollapseImports =>
        new participants.CollapseImports[treeToolbox.type](treeToolbox) :: participants.RemoveDuplicatedByWildcard :: Nil
      case _ =>
        Nil
    }.getOrElse(Nil)
    val rawPackageDefRegions = groupedPackageRegions.map {
      _.transform {
        scala.Function.chain {
          participants.SortImports ::
            SortImportSelectors ::
            new participants.RemoveUnused(rootTree, params.importsToAdd) ::
            (if (config.wildcards.nonEmpty) List(new participants.AlwaysUseWildcards[treeToolbox.type](treeToolbox)(config.wildcards)) else Nil) :::
            expandOrCollapse :::
            participants.SortImports ::
            SortImportSelectors ::
            RemoveDuplicates ::
            Nil
        }
      }
    }

    val packageDefRegions = config.collapseToWildcardConfig.map { config =>
      new transformations.collapseToWildcard(config.maxIndividualImports, config.exclude)(rawPackageDefRegions)
    }.getOrElse(rawPackageDefRegions)
    val removedDuplicates = transformations.removeScopesDuplicates(packageDefRegions ::: classDefAndDefRegions)
    val changes = removedDuplicates.map { _.print }

    val toPrint = Change.discardOverlappingChanges(changes).accepted
    Right(toPrint)
  }

}
