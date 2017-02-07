/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change
import scala.tools.refactoring.implementations.oimports.OrganizeImportsWorker
import scala.tools.refactoring.sourcegen.Formatting
import scala.util.control.NonFatal
import scala.tools.refactoring.common.InteractiveScalaCompiler

object OrganizeImports {
  /**
   * Abstract algorithms used by the implementation, extracted mostly for testing purposes
   */
  private[implementations] object Algos {
    def groupImports[ImportT](getImportExpression: ImportT => String)(groups: Seq[String], imports: Seq[ImportT]): Seq[List[ImportT]] = {
      val distinctGroups = groups.distinct

      case class Accumulator(remainingImports: Seq[ImportT] = imports, groups: Map[String, List[ImportT]] = Map()) {
        def assembleResult: Seq[List[ImportT]] = {
          val importGroups = distinctGroups.flatMap(groups.get(_))
          if (remainingImports.isEmpty) importGroups
          else importGroups :+ remainingImports.toList
        }
      }

      distinctGroups.sortBy(-_.length).foldLeft(Accumulator()) { (acc, group) =>
        val (inGroup, remaining) = acc.remainingImports.partition { imp =>
          val expr = getImportExpression(imp)
          expr.startsWith(group + ".") || expr == group
        }

        val newGroups = {
          if (inGroup.isEmpty) acc.groups
          else acc.groups + (group -> inGroup.toList)
        }

        acc.copy(remainingImports = remaining, groups = newGroups)
      }.assembleResult
    }
  }

  object Dependencies extends Enumeration {
    /**
     * Throws away all the imports and recomputes them.
     * Should NOT be used when the units has errors.
     */
    val FullyRecompute = Value

    /**
     * Recomputes the imports and removes all existing
     * imports not in the computed set. This mode preserves
     * the user's formatting of imports, and is used for
     * refactorings that modify imports but don't want
     * to fully reorganize them.
     */
    val RecomputeAndModify = Value

    /**
     * Tries to remove unneeded imports, but don't
     * throw them out when it's uncertain whether
     * they are really unneeded. Should be safe when
     * there are compile errors.
     */
    val RemoveUnneeded = Value
  }

  object ImportsStrategy extends Enumeration {
    val ExpandImports = Value
    val CollapseImports = Value
    val PreserveWildcards = Value

    def apply(strategy: String) = strategy match {
      case "expand" => Some(ExpandImports)
      case "collapse" => Some(CollapseImports)
      case "preserveWildcards" => Some(PreserveWildcards)
      case _ => None
    }
  }
  case class CollapseToWildcardConfig(maxIndividualImports: Int = 2, exclude: Set[String] = Set.empty[String])
  case class OrganizeImportsConfig(importsStrategy: Option[ImportsStrategy.Value], wildcards: Set[String] = Set.empty[String],
    groups: List[String] = Nil, scalaPackageStrategy: Boolean = false, collapseToWildcardConfig: Option[CollapseToWildcardConfig] = None)
}

/**
 * A refactoring that recomputes and reorganizes import statements in a file.
 *
 *
 */
abstract class OrganizeImports extends MultiStageRefactoring
    with InteractiveScalaCompiler
    with Formatting {

  val Dependencies = OrganizeImports.Dependencies

  class PreparationResult(val missingTypes: List[String] = Nil)

  lazy val oiWorker = new OrganizeImportsWorker[global.type](global)
  import oiWorker.global._

  /**
   * Imports that should be added are passed as tuples in the form
   * ("package.declaration", "TypeName")
   */
  class RefactoringParameters(
    val importsToAdd: List[(String, String)] = Nil,
    val deps: Dependencies.Value = Dependencies.RemoveUnneeded,
    val organizeLocalImports: Boolean = true,
    val config: Option[OrganizeImports.OrganizeImportsConfig] = None)

  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {

    def getMissingTypeNameForErroneousTree(t: Tree): String = try {
      t match {
        case Apply(Select(n: New, _), args) =>
          n.tpt.nameString
        case Apply(fun, args) =>
          fun.nameString
        case t: Select =>
          t.name.toString
        case t: Ident =>
          t.name.toString
        case t =>
          t.nameString
      }
    } catch {
      case NonFatal(_) => "Unhandled tree: " + getSimpleClassName(t) + ". You found a bug! Please report it."
    }

    val erroneousTrees = s.root.filter {
      // TypeTrees are not particularly useful on their own, so try to find a better one
      case _: TypeTree => false
      case t: Tree if t.tpe != null && t.tpe.isError => true
      case _ => false
    }

    val missingImportNames = erroneousTrees.map(getMissingTypeNameForErroneousTree).toList

    Right(new PreparationResult(missingImportNames))
  }

  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    params.config.map { _ =>
      oiWorker.organizeAll[global.type](global)(selection, params, this)
    }.getOrElse(Right(Nil))
  }
}
