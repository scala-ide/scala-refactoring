package scala.tools.refactoring
package implementations

import scala.tools.refactoring.analysis.CompilationUnitDependencies
import scala.tools.refactoring.common.TreeExtractors
import scala.tools.refactoring.common.NewFileChange
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.transformation.TreeFactory
import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.common.TextChange
import scala.reflect.internal.util.SourceFile

abstract class MoveClass extends MultiStageRefactoring with TreeFactory with analysis.Indexes with TreeExtractors with InteractiveScalaCompiler with CompilationUnitDependencies with ImportsHelper {

  import global._

  /**
   * Returns Some if the user selected a single ImplDef in the source file.
   * None means that all ImplDefs should be moved.
   */
  type PreparationResult = Option[ImplDef]

  /**
   * We don't really need the preparation result in the refactoring, instead
   * the user has the option of ignoring the selected ImplDef, therefore we
   * take a copy of the PreparationResult as a parameter.
   * */
  case class RefactoringParameters(packageName: String, moveSingleImpl: PreparationResult)

  /**
   * We can only move classes from files that contain a single package.
   * Note that
   *
   *   package a
   *   package b
   *
   * is considered a single package, but
   *
   *   package a
   *   class A
   *   package b
   *
   * is not.
   */
  def prepare(s: Selection) = {
    def hasSinglePackageDeclaration = topPackageDef(s.root.asInstanceOf[PackageDef]) match {
      case PackageDef(_, stats) => stats forall (stmt => stmt.isInstanceOf[ImplDef] || stmt.isInstanceOf[Import])
      case _ => false
    }

    val topLevelImpls = topLevelImplDefs(s)

    def hasSingleTopLevelImpl = topLevelImpls.size == 1

    def hasToplevelClassAndCompanion = topLevelImpls match {
      case fst :: snd :: Nil =>
        fst.symbol.companionSymbol == snd.symbol
      case _ => false
    }

    s.findSelectedOfType[ImplDef] match {
      // If there is only one ImplDef in the file, we simply move
      // all impls. This doesn't matter from the refactoring's
      // perspective, but is used in the IDE to determine if it's
      // possible to split a class from a multiple-definition file.
      case Some(singleImplDef) if hasSingleTopLevelImpl =>
        Right(None)
      case Some(singleImplDef) if hasToplevelClassAndCompanion =>
        Right(None)
      case Some(singleImplDef) =>
        Right(Some(singleImplDef))
      case None if hasSinglePackageDeclaration =>
        Right(None) // Move all ImplDefs
      case _ =>
        Left(PreparationError("Files with multiple packages cannot be moved."))
    }
  }

  /**
   * Returns all the statements that will be moved in this refactoring.
   *
   * Note: it will also return Import trees, not just ImplDefs.
   */
  def statsToMove(selection: Selection, parameters: RefactoringParameters) = {
    if(moveOnlyPartOfSourceFile(selection, parameters)) {
      parameters.moveSingleImpl.toList
    } else {
      topLevelStats(selection)
    }
  }

  def perform(selection: Selection, preparationResult: PreparationResult, parameters: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    val toMove = parameters.moveSingleImpl

    val ancestors = toMove.toList flatMap ancestorSymbols

    trace("Selected ImplDef: %s, in package %s, move to %s", toMove map(_.nameString) getOrElse "ALL", ancestors map (_.nameString) mkString ("."), parameters)

    /*
     * We need to handle two different cases:
     *
     *  1) A single class from the existing file is moved to a new one and
     *     there are other classes remaining in the file.
     *
     *  2) The complete file is moved, this is easier because we simply move
     *     the file and don't need to create a new source file.
     * */
    val movedClassChanges = if(moveOnlyPartOfSourceFile(selection, parameters)) {

      val newFileChanges = {
        val moveClass = {
          val stats = statsToMove(selection, parameters)
          createRenamedTargetPackageTransformation(parameters, stats, toMove.get)
        }
        val changes = transformFile(selection.file, moveClass)
        changes map (_.toNewFile(parameters.packageName))
      }

      newFileChanges ++ removeClassFromOldFileAndAddImportToNewIfNecessary(selection, parameters)

    } else {
      val stats = statsToMove(selection, parameters)
      val transformation = createRenamedTargetPackageTransformation(parameters, stats, selection.root)
      transformFile(selection.file, transformation)
    }

    /*
     * We need to adapt the imports of all the files that reference one of the moved classes.
     * This include imports to the moved classes and fully qualified names.
     * */
    val otherFiles = adaptDependentFiles(selection, toMove, parameters.packageName)

    Right(movedClassChanges ++ otherFiles)
  }

  private def addRequiredImportsForExtractedClass(toMove: Tree, targetPackageName: String) = {
    addRequiredImports(Some(toMove), Some(targetPackageName))
  }

  /**
   * Returns a transformation that creates the contents of the target file.
   * */
  private def createRenamedTargetPackageTransformation(parameters: RefactoringParameters, implsToMove: List[Tree], importsFor: Tree) = {

    val targetPackages = parameters.packageName.split("\\.").toList

    val findFirstPackageToRename = filter {
      case pkg: PackageDef if pkg.stats.contains(implsToMove.head /*!*/) =>
        true
      case pkg: PackageDef =>
        val packages = ancestorSymbols(pkg) map (_.nameString)
        val found = !(targetPackages startsWith packages)
        found
    }

    val changePackageDeclaration = transform {
      case pkg @ PackageDef(pid, stats) =>

        val surroundingPackages = originalParentOf(pkg) match {
          case Some(parentPkg: PackageDef) =>
            ancestorSymbols(parentPkg) map (_.nameString)
          case _ => Nil
        }

        val newPid = if(targetPackages.startsWith(surroundingPackages)) {
          (targetPackages.drop(surroundingPackages.size))
        } else {
          targetPackages
        }

        // move to the default package:
        if(newPid == List("")) {
          PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), implsToMove) replaces pkg
        } else if(newPid.isEmpty) {
          PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), implsToMove) // don't `replace` to get rid of empty lines
        } else {
          PackageDef(pid = Ident(newPid mkString ".") replaces pid, stats = implsToMove) replaces pkg
        }
    }

    val insertImports = addRequiredImportsForExtractedClass(importsFor, parameters.packageName)

    traverseAndTransformAll(findFirstPackageToRename &> changePackageDeclaration) &> insertImports
  }

  /**
   * Returns the list of changes that adapt the old file (only when we don't move the complete file)
   * */
  private def removeClassFromOldFileAndAddImportToNewIfNecessary(selection: Selection, parameters: RefactoringParameters) = {

    val toMove = parameters.moveSingleImpl.get

    val referencesToMovedClass = index.references(toMove.symbol)

    val referencesInOriginalFile = referencesToMovedClass.filter(_.pos.source.file == selection.file)

    def hasRelativeReferenceToMovedClass = referencesInOriginalFile.exists {
      // TODO check if the complete qualifier has a range!
      case Select(qual, _) if qual.pos.isRange => false
      // check the position to exclude self references
      case t: RefTree if toMove.pos.includes(t.pos) => false
      case _ => true
    }

    val removeClassFromOldFile = replaceTree(toMove, EmptyTree)

    val trans = if(hasRelativeReferenceToMovedClass) {
      removeClassFromOldFile &> addImportTransformation(List(parameters.packageName + "." + toMove.nameString))
    } else {
      removeClassFromOldFile
    }

    transformFile(selection.file, trans)
  }

  private def topLevelStats(selection: Selection): List[Tree] = {
    topPackageDef(selection.root.asInstanceOf[PackageDef]).stats collect {
      case impl: ImplDef => impl
      case imp:  Import  => imp
    }
  }

  private def topLevelImplDefs(selection: Selection): List[ImplDef] = {
    topLevelStats(selection) collect {
      case impl: ImplDef => impl
    }
  }

  private def moveOnlyPartOfSourceFile(selection: Selection, parameters: RefactoringParameters) = {
    parameters.moveSingleImpl.isDefined && topLevelImplDefs(selection).size > 1
  }

  private def adaptDependentFiles(selection: Selection, toMove: Option[ImplDef], newFullPackageName: String): Iterable[Change] = {

    def referencesToMovedClasses(moved: List[ImplDef]): Map[SourceFile, List[(ImplDef, List[Tree])]] = {

      // `moved` can contain duplicates, e.g. a class and its companion object
      val distinctImplsToMove = moved.groupBy(_.nameString).mapValues(_.head).values.toList.sortBy(_.nameString)

      val referencesPerFile = collection.mutable.Map[SourceFile, List[(ImplDef, List[Tree])]]()

      def addToMap(impl: ImplDef) {
        index.references(impl.symbol) groupBy (_.pos.source) foreach {
          case (src, references) if src.file != selection.file =>
            val old = referencesPerFile.getOrElse(src, List[(ImplDef, List[Tree])]())
            referencesPerFile(src) = (impl, references) :: old
          case _ => ()
        }
      }

      distinctImplsToMove foreach addToMap

      referencesPerFile.toMap
    }

    val referencesFromOtherFiles = referencesToMovedClasses {
      toMove map (List(_)) getOrElse topLevelImplDefs(selection)
    } flatMap {
      case (sourceFile, entries) => entries.toList map {
        case (implDef, references) =>
          (sourceFile, implDef, references)
      }
    }

    referencesFromOtherFiles flatMap {
      case (sourceFile, implDef, references) =>

        val referencedName = implDef.nameString

        val alreadyHasImportSelector = references.exists(_.isInstanceOf[ImportSelectorTree])

        def hasReferenceWithoutFullName = references.exists {
          // TODO check if the complete qualifier has a Range!
          case Select(qual, _) if qual.pos.isRange => false
          case t: RefTree => true
        }

        if(!alreadyHasImportSelector && hasReferenceWithoutFullName) {
          val addImport = new AddImportStatement { val global = MoveClass.this.global }
          addImport.addImport(sourceFile.file, newFullPackageName + "." + referencedName)
        } else {

          def hasMovedName(s: ImportSelector) = s.name.toString == referencedName

          val adaptImports = transform {

            /*
             * The import has a single selector that imports the class we move.
             * */
            case pkg @ PackageDef(_, stats) if stats.exists {
              case Import(_, selector :: Nil) => hasMovedName(selector)
              case _ => false
            } =>
              /*
               * We are lucky and can replace the import expression.
               * */
              pkg copy (stats = stats map {
                case imp @ Import(_, selector :: Nil) if hasMovedName(selector) =>
                  imp copy (expr = Ident(newFullPackageName)) replaces imp
                case stmt => stmt
              }) copyAttrs pkg

            /*
             * The import has multiple selectors with one of them being the class we move.
             * */
            case pkg @ PackageDef(_, stats) if stats.exists {
              case Import(_, selectors) => selectors.exists(hasMovedName)
              case _ => false
            } =>
              /*
               * Remove the obsolete selector and add a new import with the new package name.
               * */
              pkg copy (stats = stats flatMap {
                case imp @ Import(_, selectors) if selectors.exists(hasMovedName) =>
                  val selector = selectors.find(hasMovedName).get
                  List(
                      imp copy (selectors = selectors.filterNot(_ == selector)) replaces imp,
                      Import(Ident(newFullPackageName), selector :: Nil))
                case stmt =>
                  List(stmt)
              }) copyAttrs pkg

            case s @ Select(qualifier, _) if references.contains(s) &&
                qualifier.pos.isRange /* qualifier is visible in the source code */ =>
              s copy (qualifier = Ident(newFullPackageName)) replaces s

            case ref: Ident if references.contains(ref) && !alreadyHasImportSelector =>
              Ident(newFullPackageName + "." + ref.name)
          }

          transformFile(sourceFile.file, traverseAndTransformAll(adaptImports))
        }
    }
  }
}
