package scala.tools.refactoring
package implementations

import scala.tools.nsc.util.SourceFile
import scala.tools.refactoring.analysis.CompilationUnitDependencies
import scala.tools.refactoring.common.{TreeExtractors, NewFileChange, InteractiveScalaCompiler, Change}
import scala.tools.refactoring.transformation.TreeFactory
import scala.collection.mutable.ListBuffer

abstract class MoveClass extends MultiStageRefactoring with TreeFactory with analysis.Indexes with TreeExtractors with InteractiveScalaCompiler with CompilationUnitDependencies {

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

    s.findSelectedOfType[ImplDef] match {
      case Some(singleImplDef) => Right(Some(singleImplDef))
      case None if hasSinglePackageDeclaration => Right(None) // Move all ImplDefs
      case _ => Left(PreparationError("Files with multiple packages cannot be moved."))
    }
  }

  def perform(selection: Selection, preparationResult: PreparationResult, parameters: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    val toMove = parameters.moveSingleImpl

    val ancestors = toMove.toList flatMap ancestorSymbols

    trace("Selected ImplDef: %s, in package %s, move to %s", toMove map(_.nameString) getOrElse "ALL", ancestors map (_.nameString) mkString ("."), parameters)

    val targetPackageName = parameters.packageName

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
          val insertImports = addRequiredImportsForExtractedClass(toMove.get, targetPackageName)
          val statsToMove = parameters.moveSingleImpl.toList
          createRenamedTargetPackageTransformation(parameters, statsToMove) &> insertImports
        }
        val changes = transformFile(selection.file, moveClass)
        changes map {
          case change @ Change(file, from, to, src) =>
            // TODO: Apply change so we get the complete source file
            val src2 = Change.applyChanges(List(change), new String(change.underlyingSource.get.content))
            new NewFileChange(targetPackageName, file, from, to, src2)
        }
      }

      newFileChanges ++ removeClassFromOldFileAndAddImportToNewIfNecessary(selection, parameters)

    } else {
      val statsToMove = topLevelStats(selection)
      val transformation = createRenamedTargetPackageTransformation(parameters, statsToMove)
      transformFile(selection.file, transformation)
    }

    /*
     * We need to adapt the imports of all the files that reference one of the moved classes.
     * This include imports to the moved classes and fully qualified names.
     * */
    val otherFiles = adaptDependentFiles(selection, toMove, targetPackageName)

    Right(movedClassChanges ++ otherFiles)
  }

  private def addRequiredImportsForExtractedClass(toMove: ImplDef, targetPackageName: String) = traverseAndTransformAll {
    transform {
      case pkg @ PackageDef(_, stats) if stats contains toMove=>
        val dependencies = neededImports(toMove) filterNot (_.symbol == toMove.symbol)
        val requiredImports = mkImportTrees(dependencies, targetPackageName)
        pkg copy (stats = requiredImports ++ stats) replaces pkg
    }
  }

  /**
   * Returns a transformation that creates the contents of the target file.
   * */
  private def createRenamedTargetPackageTransformation(parameters: RefactoringParameters, implsToMove: List[Tree]) = {

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

        if(newPid.isEmpty) {
          PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), implsToMove)
        } else {
          PackageDef(pid = Ident(newPid mkString ".") replaces pid, stats = implsToMove) replaces pkg
        }
    }

    traverseAndTransformAll(findFirstPackageToRename &> changePackageDeclaration)
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

    //val changes = createChanges(trans(abstractFileToTree(selection.file)).toList)

    val t = transformFile(selection.file, trans)
    t
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

      val referencesPerFile = collection.mutable.Map[SourceFile, List[(ImplDef, List[Tree])]]()

      def addToMap(impl: ImplDef) {
        index.references(impl.symbol) groupBy (_.pos.source) foreach {
          case (src, references) if src.file != selection.file =>
            val old = referencesPerFile.getOrElse(src, List[(ImplDef, List[Tree])]())
            referencesPerFile(src) = (impl, references) :: old
          case _ => ()
        }
      }

      moved foreach addToMap

      referencesPerFile.toMap
    }

    val referencesFromOtherFiles = referencesToMovedClasses {
      toMove map (List(_)) getOrElse topLevelImplDefs(selection)
    } flatMap {
      case (sourceFile, entries) => entries.toList map {
        case (implDef, references) => (sourceFile, implDef, references)
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
