package scala.tools.refactoring.implementations.oimports

import scala.tools.refactoring.common.Change
import scala.tools.refactoring.implementations.OrganizeImports
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.sourcegen.Formatting
import scala.tools.refactoring.transformation.TreeTransformations

class OrganizeImportsWorker[G <: Global](val global: G) extends InteractiveScalaCompiler
    with TreeTransformations
    with Selections {
  import global._

  lazy val treeToolbox = new TreeToolbox[global.type](global)
  lazy val regionContext = new RegionTransformationsContext[global.type](global)
  lazy val transformations = new regionContext.RegionTransformations[treeToolbox.type](treeToolbox)
  lazy val participants = new ImportParticipants[global.type](global)
  lazy val defImportsOrganizer = new DefImportsOrganizer[treeToolbox.global.type, treeToolbox.type](treeToolbox)
  lazy val classDefImportsOrganizer = new ClassDefImportsOrganizer[treeToolbox.global.type, treeToolbox.type](treeToolbox)
  lazy val packageDefImportsOrganizer = new PackageDefImportsOrganizer[treeToolbox.global.type, treeToolbox.type](treeToolbox)

  private def organizeLocalImports(root: Tree, formatting: Formatting): List[treeToolbox.Region] = {
    import participants._
    val defRegions = defImportsOrganizer.transformTreeToRegions(root, formatting).map {
      _.transform {
        scala.Function.chain {
          RemoveDuplicatedByWildcard ::
            new RemoveUnused(root) ::
            RemoveDuplicates ::
            SortImportSelectors ::
            SortImports ::
            Nil
        }
      }
    }

    val classDefRegions = classDefImportsOrganizer.transformTreeToRegions(root, formatting).map {
      _.transform {
        scala.Function.chain {
          RemoveDuplicatedByWildcard ::
            new RemoveUnused(root) ::
            RemoveDuplicates ::
            SortImportSelectors ::
            SortImports ::
            Nil
        }
      }
    }
    classDefRegions ::: defRegions
  }

  type CanCastTree[SG <: Global] = SG =:= global.type
  def organizeAll[SG <: Global : CanCastTree](g: SG)(selection: Selections#Selection, params: OrganizeImports#RefactoringParameters, formatting: Formatting) = {
    val rootTree = abstractFileToTree(selection.file).asInstanceOf[Tree]
    val rootSelection = selection.root.asInstanceOf[Tree]

    val classDefAndDefRegions = if (params.organizeLocalImports) {
      organizeLocalImports(rootTree, formatting)
    } else {
      Nil
    }

    val config = params.config.get

    val rawPackageRegions = packageDefImportsOrganizer.transformTreeToRegions(rootTree, formatting)

    import OrganizeImports.Dependencies
    val packageRegions = params.deps match {
      case Dependencies.FullyRecompute =>
        new transformations.addExpandedImports(rootSelection)(rawPackageRegions)
      case Dependencies.RecomputeAndModify =>
        new transformations.recomputeAndModifyUnused(rootSelection)(rawPackageRegions)
      case Dependencies.RemoveUnneeded =>
        new transformations.addNewImports(params.importsToAdd)(rawPackageRegions, rootSelection, formatting)
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
            participants.SortImportSelectors ::
            new participants.RemoveUnused(rootTree, params.importsToAdd) ::
            (if (config.wildcards.nonEmpty) List(new participants.AlwaysUseWildcards[treeToolbox.type](treeToolbox)(config.wildcards)) else Nil) :::
            expandOrCollapse :::
            participants.SortImports ::
            participants.SortImportSelectors ::
            participants.RemoveDuplicates ::
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
