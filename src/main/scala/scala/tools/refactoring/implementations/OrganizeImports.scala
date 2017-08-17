/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.implementations.oimports.OrganizeImportsWorker
import scala.tools.refactoring.sourcegen.Formatting
import scala.util.control.NonFatal

object OrganizeImports {
  val DefaultGroup = "*"
  val InGroupDelimiter = ","
  /**
   * Abstract algorithms used by the implementation, extracted mostly for testing purposes
   */
  private[implementations] object Algos {
    def groupImports[ImportT](getImportExpression: ImportT => String)(groups: Seq[String], imports: Seq[ImportT]): Seq[List[ImportT]] = {
      val distinctGroups = groups.map(_.split(InGroupDelimiter).sorted.mkString(InGroupDelimiter)).distinct
      val groupImportsMap = distinctGroups.map(_ -> scala.collection.mutable.ListBuffer.empty[ImportT]).toMap
      val assigned = imports.foldLeft(groupImportsMap) { (groupImportsMap, imp) =>
        val expr = getImportExpression(imp)
        def isInGroup(group: String) = expr.startsWith(group + ".") || expr == group
        distinctGroups.foldLeft[Option[String]](None) { (longestGroup, group) =>
          val potential = group.split(InGroupDelimiter).exists { isInGroup }
          if (potential) {
            val locallyLongest = group.split(InGroupDelimiter).foldLeft(0) { (locallyLongest, group) =>
              if (isInGroup(group) && group.length > locallyLongest) {
                group.length
              } else locallyLongest
            }
            longestGroup.map { lg =>
              val longest = lg.split(InGroupDelimiter).foldLeft(0) { (longest, g) =>
                if (isInGroup(g) && g.length > longest)
                  g.length
                else longest
              }
              if (longest < locallyLongest)
                group
              else lg
            }.orElse(Option(group))
          } else longestGroup
        }.map { mostSpecificGroup =>
          groupImportsMap(mostSpecificGroup) += imp
        }
        groupImportsMap
      }
      val unassigned = {
        val a = assigned.values.toList.flatten.map(getImportExpression)
        imports.filterNot { imp => a.contains(getImportExpression(imp)) }
      }
      (if (assigned.keySet(DefaultGroup)) {
        assigned(DefaultGroup) ++= unassigned
        distinctGroups.foldRight(Seq.empty[List[ImportT]]) { (key, acc) =>
          assigned(key).toList +: acc
        }
      } else {
        distinctGroups.foldRight(Seq.empty[List[ImportT]]) { (key, acc) =>
          assigned(key).toList +: acc
        } :+ unassigned.toList
      }).filter(_.nonEmpty)
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
