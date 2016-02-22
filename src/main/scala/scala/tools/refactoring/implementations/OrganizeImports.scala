/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.TreeTraverser
import common.Change
import transformation.TreeFactory
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
                                                             with common.TreeExtractors {

  import OrganizeImports.Algos
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

  case class GroupImports(groups: List[String]) extends Participant {
    protected def doApply(trees: List[Import]) = {
      def getImportExpression(imp: Import) = imp.expr.toString
      val allImports = Algos.groupImports(getImportExpression)(groups, trees)
      val spacer = List(Import(PlainText.BlankLine, Nil))

      if (allImports.size > 1) {
        allImports.reduceLeft(_ ++ spacer ++ _)
      } else {
        allImports.flatten.toList
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
          imp.copy(selectors = removeDuplicates(selectors).sortBy(_.name.toString))
      }
    }
  }

  class RecomputeAndModifyUnused(allNeededImports: List[Tree]) extends Participant {

    protected def doApply(trees: List[Import]) = {

      val importsNames = allNeededImports map importAsString

      trees flatMap {
        case imp @ Import(expr, selectors) =>
          val pkgName = importAsString(expr) + "."

          val neededSelectors = selectors.filter { selector =>
            selector.name == nme.WILDCARD || importsNames.contains(pkgName + selector.name)
          }

          // If parts of the expr aren't ranges, then we have an import that depends on an
          // other import (see OrganizeImportsRecomputeAndModifyTest#importDependingOnImport)
          def exprIsAllRangePos = {
            // no Tree#forall, so we use double-negative
            !expr.exists(t => !t.pos.isRange)
          }

          def invisiblePartIsDefaultImported = {
            findDeepestNeededSelect(expr) exists isQualifierDefaultImported
          }

          if (neededSelectors.size == selectors.size && (exprIsAllRangePos || invisiblePartIsDefaultImported)) {
            Some(imp)
          } else if (neededSelectors.nonEmpty) {

            /* Imports from the scala package don't have to start with `scala`,
             * and we don't want to enforce this, so we just keep the expr as
             * it is. On the other hand, if the import is not from the `scala`
             * package, we set all positions to NoPos to make the visible in
             * the generated code.
             */
            val fullExpr = if (isImportFromScalaPackage(expr)) {
              expr
            } else {
              stripPositions(expr)
            }

            Some(Import(fullExpr, neededSelectors))
          } else {
            None
          }
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

          t copy (expr = transformation(expr).get /*safe becaues of pattern guard*/ )
        case t => t
      }
    }
  }

  class AddNewImports(importsToAdd: List[(String, String)]) extends Participant {
    protected def doApply(trees: List[Import]) = {
      val newImports = importsToAdd map (mkImportFromStrings _).tupled
      newImports ::: trees
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
    val deps: Dependencies.Value = Dependencies.RemoveUnneeded)

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
    val unit = compilationUnitOfFile(selection.pos.source.file).get
    val importStrategy = params.deps match {
      case Dependencies.FullyRecompute =>

        val enclosingPackage = selection.root match {
          case root: PackageDef =>
            val rootPackage = topPackageDef(root)
            ancestorSymbols(rootPackage).map(_.nameString).mkString(".")
          case _ => ""
        }

        new FindNeededImports(selection.root, enclosingPackage) :: SortImports :: Nil

      case Dependencies.RecomputeAndModify =>
        new RecomputeAndModifyUnused(neededImports(selection.root)) :: RemoveDuplicates :: Nil

      case Dependencies.RemoveUnneeded =>
        new AddNewImports(params.importsToAdd) :: SortImports :: new RemoveUnused(unit, params.importsToAdd) :: Nil
    }

    val participants = importStrategy ::: params.options

    val organizeImports = findBestPackageForImports &> transformation[(PackageDef, List[Import], List[Tree]), Tree] {
      case (p, existingImports, others) =>
        val imports = scala.Function.chain(participants)(existingImports)
        p copy (stats = imports ::: others) replaces p
    } &> transformation[Tree, Tree] {
      case p: PackageDef =>
        MethodImportsOrganizer.organizeImportsInMethodBlocks(p).replaces(p)
    }

    Right(transformFile(selection.file, organizeImports |> topdown(matchingChildren(organizeImports))))
  }

  object MethodImportsOrganizer {
    val methodOrganizeImportsParticipants = new NotPackageImportParticipants(OrganizeImports.this.global, OrganizeImports.this)
    import methodOrganizeImportsParticipants.RemoveDuplicatedByWildcard

    private def organizeImportsIfNoImportInSameLine(imports: List[Import])(organizeImports: List[Import] => List[Import]): List[Import] = {
      val importsWithPosition = imports.filter { _.pos.isDefined }
      if (importsWithPosition.nonEmpty &&
        importsWithPosition.size != importsWithPosition.map { _.pos.line }.distinct.size)
        imports
      else organizeImports(imports)
    }

    private def organizeGroupedImports(stats: List[Tree])(importsOrganizer: List[Import] => List[Import])(transformer: Transformer): List[Tree] = {
      val treeListTurnedToListOfSingleTreeElementList = stats.map { tree => List(tree) }
      val mergeNeighborImportLists = treeListTurnedToListOfSingleTreeElementList.foldLeft(List.empty[List[Tree]]) { (zero, elem) =>
        elem.head match {
          case addToProceedingImports: Import =>
            def isZeroHeadAListOfImports(zeroHead: List[Tree]) = zeroHead.forall { _.isInstanceOf[Import] }
            zero match {
              case head :: tail if isZeroHeadAListOfImports(head) => (addToProceedingImports :: head) :: tail
              case _ => elem :: zero
            }
          case _ => elem :: zero
        }
      }
      val restoreOrderAfterFoldLeft = mergeNeighborImportLists.reverse.map {
        _.reverse
      }
      val applyImportsOrganizerToImportsLists = restoreOrderAfterFoldLeft.map {
        case imps @ (head: Import) :: _ =>
          organizeImportsIfNoImportInSameLine(imps.asInstanceOf[List[Import]])(importsOrganizer)
        case t => t.map { transformer.transform }
      }
      applyImportsOrganizerToImportsLists.flatten
    }

    def organizeImportsInMethodBlocks(tree: Tree): Tree = new Transformer {
      override def transform(t: Tree) = t match {
        case b @ Block(stats, _) if currentOwner.isMethod && !currentOwner.isLazy =>
          val participants = (new methodOrganizeImportsParticipants.RemoveUnused(b)).asInstanceOf[Participant] ::
            RemoveDuplicatedByWildcard.asInstanceOf[Participant] ::
            RemoveDuplicates :: SortImportSelectors :: SortImports :: Nil
          val importsOrganizer = scala.Function.chain(participants)
          val reorganizedStats = organizeGroupedImports(stats)(importsOrganizer)(this)
          b.copy(stats = reorganizedStats).replaces(b)
        case skipPlainText: PlainText => skipPlainText
        case t => super.transform(t)
      }
    }.transform(tree)
  }
}
