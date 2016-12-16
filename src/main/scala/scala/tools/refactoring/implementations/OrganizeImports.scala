/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TreeTraverser
import scala.tools.refactoring.implementations.oimports.OrganizeImportsWorker
import scala.tools.refactoring.sourcegen.Formatting
import scala.tools.refactoring.transformation.TreeFactory
import scala.util.control.NonFatal

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
abstract class OrganizeImports extends MultiStageRefactoring with TreeFactory
    with TreeTraverser
    with UnusedImportsFinder
    with analysis.CompilationUnitDependencies
    with common.InteractiveScalaCompiler
    with common.TreeExtractors
    with Formatting {

  import global._

  val Dependencies = OrganizeImports.Dependencies

  class PreparationResult(val missingTypes: List[String] = Nil)

  trait Participant extends (List[Import] => List[Import]) {
    protected final def importAsString(t: Tree): String = {
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

    protected final def stripPositions(t: Tree) = {
      topdown(setNoPosition) apply t.duplicate getOrElse t
    }

    protected final def isImportFromScalaPackage(expr: Tree) = {
      expr.filter(_ => true).lastOption exists {
        case Ident(nme.scala_) => true
        case _ => false
      }
    }

    protected def doApply(trees: List[Import]): List[Import]

    final def apply(trees: List[Import]): List[Import] = {
      doApply(trees) \\ { res =>
        trace(s"$this:")
        trees.foreach(trace("-- %s", _))
        res.foreach(trace("++ %s", _))
      }
    }

    override def toString = s"Participant[$name]"

    private def name = getSimpleClassName(this)
  }

  object CollapseImports extends Participant {
    protected def doApply(trees: List[Import]) = {
      trees.foldRight(Nil: List[Import]) {
        case (imp: Import, x :: xs) if createText(imp.expr) == createText(x.expr) =>
          x.copy(selectors = x.selectors ::: imp.selectors).setPos(x.pos) :: xs
        case (imp: Import, xs) =>
          imp :: xs
      }
    }
  }

  object ExpandImports extends Participant {
    protected def doApply(trees: List[Import]) = {
      trees flatMap {
        case imp @ Import(_, selectors) if selectors.exists(wildcardImport) =>
          List(imp)
        case imp @ Import(_, selector :: Nil) =>
          List(imp)
        case Import(expr, selectors) =>
          selectors map {
            selector => Import(expr, selector :: Nil)
          }
      }
    }
  }

  private def renames(i: ImportSelector) = i.rename != null && i.name != i.rename

  object SimplifyWildcards extends Participant {
    protected def doApply(trees: List[Import]) = {
      trees map {
        case imp @ Import(_, selectors) if selectors.exists(wildcardImport) && !selectors.exists(renames) =>
          imp.copy(selectors = selectors.filter(wildcardImport)).setPos(imp.pos)
        case imp =>
          imp
      }
    }
  }

  object SortImports extends Participant {

    def asText(t: Tree) = createText(stripPositions(t))

    protected def doApply(trees: List[Import]) = {
      trees.sortBy {
        case i @ Import(expr, selector :: Nil) if !wildcardImport(selector) =>
          asText(expr) + "." + selector.name.toString
        case i @ Import(expr, selectors) =>
          asText(expr)
      }
    }
  }

  case class AlwaysUseWildcards(imports: Set[String]) extends Participant {
    protected def doApply(trees: List[Import]) = {
      val seen = collection.mutable.HashSet[String]()
      trees flatMap {
        case imp @ Import(qual, selectors) if imports.contains(asSelectorString(qual)) && !selectors.exists(renames) =>
          if (seen.contains(asSelectorString(qual))) {
            None
          } else {
            seen += asSelectorString(qual)
            Some(Import(qual, List(ImportSelector(nme.WILDCARD, -1, nme.WILDCARD, -1))).copyAttrs(imp))
          }
        case t => Some(t)
      }
    }
  }

  object RemoveDuplicates extends Participant {
    protected def doApply(trees: List[Import]) = {
      trees.foldLeft(Nil: List[Import]) {
        case (rest, imp) if rest.exists(t => t.toString == imp.toString) =>
          rest
        case (rest, imp) => imp :: rest
      }.reverse
    }
  }

  object SortImportSelectors extends Participant {
    protected def doApply(trees: List[Import]) = {
      trees.map {
        case imp @ Import(_, selectors :: Nil) => imp
        case imp @ Import(_, selectors) if selectors.exists(wildcardImport) => imp
        case imp @ Import(_, selectors) =>
          def removeDuplicates(l: List[ImportSelector]) = {
            l.groupBy(_.name.toString).map(_._2.head).toList
          }
          imp.copy(selectors = removeDuplicates(selectors).sortBy(_.name.toString)).setPos(imp.pos)
      }
    }
  }

  class RemoveUnused(unit: RichCompilationUnit, importsToAdd: List[(String, String)]) extends Participant {
    protected def doApply(trees: List[Import]) = {
      val additionallyImportedTypes = importsToAdd.unzip._2
      trees map {
        case imp @ Import(expr, selectors) =>

          val neededSelectors = selectors.filter { s =>
            neededImportSelector(unit, expr, s) ||
              additionallyImportedTypes.contains(s.name.toString)
          }

          if (neededSelectors.nonEmpty) {
            Import(stripPositions(expr), neededSelectors)
          } else {
            Import(EmptyTree, Nil)
          }
      }
    }
  }

  class FindNeededImports(root: Tree, enclosingPackage: String) extends Participant {
    protected def doApply(trees: List[Import]) = {
      mkImportTrees(neededImports(root), enclosingPackage)
    }
  }

  object PrependScalaPackage extends Participant {
    protected def doApply(trees: List[Import]) = {
      trees map {
        case t @ Import(expr, _) if isImportFromScalaPackage(expr) =>
          // Setting all positions to NoPosition forces the pretty printer
          // to print the complete selector including the leading `scala`
          t copy (expr = stripPositions(expr))
        case t => t
      }
    }
  }

  object DropScalaPackage extends Participant {
    protected def doApply(trees: List[Import]) = {
      trees map {
        case t @ Import(expr, name) if isImportFromScalaPackage(expr) =>

          val transformation = traverseAndTransformAll {
            transform {
              case t @ Ident(nme.scala_) =>
                Ident(nme.scala_) copyAttrs t setPos Invisible
            }
          }

          t copy (expr = transformation(expr).get /*safe because of pattern guard*/ )
        case t => t
      }
    }
  }

  case class CollapseSelectorsToWildcard(maxIndividualImports: Int = 2, exclude: Set[String] = Set()) extends Participant {
    protected def doApply(trees: List[Import]) = {

      // Don't collapse if newly imported names collide with names currently
      // imported by wildcards.
      val wildcardImportedNames = collection.mutable.HashSet[Name]()
      wildcardImportedNames ++= getWildcardImportedNames(trees)

      trees.map {
        case imp @ Import(exp, selectors) if selectors.size > maxIndividualImports &&
          !exclude.contains(asSelectorString(exp)) &&
          !selectors.exists(wildcardImport) &&
          !selectors.exists(renames) &&
          canSafelyCollapse(imp, wildcardImportedNames) =>
          // This replacement causes previously explicitly imported names to be imported,
          // which lowers their precedence. Subsequent wildcard imports should not collide
          // with these.
          wildcardImportedNames ++= selectors.collect {
            case ImportSelector(name, _, _, _) if name != nme.WILDCARD => name
          }
          imp.copy(selectors = List(ImportSelector(nme.WILDCARD, -1, nme.WILDCARD, -1)))
        case imp =>
          imp
      }
    }

    def getWildcardImportedNames(trees: List[Import]) = {
      trees flatMap {
        case Import(exp, selectors) if selectors.exists(wildcardImport) =>
          val all = exp.tpe.members.map(_.name).toSet
          val explicit = selectors.collect {
            case ImportSelector(name, _, _, _) if name != nme.WILDCARD => name
          }.toSet
          all filterNot explicit.contains
        case _ => Nil
      }
    }

    def canSafelyCollapse(imp: Import, wildcardImportedNames: collection.Set[Name]) = {
      val importedSymbolNames = imp.selectors.map(_.name)
      val newSymbols = imp.expr.tpe.members.filterNot(symbol => importedSymbolNames.contains(symbol.name))
      val newNames = newSymbols.map(_.name)

      !newNames.exists(wildcardImportedNames.contains) && !newSymbols.exists(_.isImplicit)
    }
  }

  def DefaultOptions = List(CollapseImports, SimplifyWildcards, SortImportSelectors, SortImports)

  /**
   * Imports that should be added are passed as tuples in the form
   * ("package.declaration", "TypeName")
   */
  class RefactoringParameters(
    val importsToAdd: List[(String, String)] = Nil,
    val options: List[Participant] = DefaultOptions,
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
    val oiWorker = new OrganizeImportsWorker[this.type](this)
    params.config.map { _ =>
      oiWorker.organizeAll(selection, params)
    }.getOrElse(Right(Nil))
  }
}
