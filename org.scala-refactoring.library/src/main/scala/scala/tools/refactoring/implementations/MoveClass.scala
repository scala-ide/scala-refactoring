package scala.tools.refactoring
package implementations

import scala.tools.nsc.util.SourceFile
import scala.tools.refactoring.analysis.CompilationUnitDependencies
import scala.tools.refactoring.common.{TreeExtractors, NewFileChange, InteractiveScalaCompiler, Change}
import scala.tools.refactoring.transformation.TreeFactory
import scala.collection.mutable.ListBuffer

abstract class MoveClass extends MultiStageRefactoring with TreeFactory with analysis.Indexes with TreeExtractors with InteractiveScalaCompiler with CompilationUnitDependencies {
  
  import global._
  
  type PreparationResult = ImplDef
  
  /**
   * KeepFile: the whole file is being moved
   * NewFile: the class is moved to a new file, we need to remove it from the old file and add it to a new one (two changes)
   * ExistingFile: the class is moved into an already existing file, we need to update two files. the package name is taken from the target file
   */
  sealed trait Target
  case class KeepFile (name: String) extends Target
  case class NewFile  (name: String) extends Target
  //case class ExistingFile(targetFile: SourceFile) extends Target
  
  type RefactoringParameters = Target
  
  def prepare(s: Selection) = {
    
    def getSingleImplDef: Option[ImplDef] = {
      s.root match {
        case root: PackageDef => 
          topPackageDef(root) match {
            case PackageDef(_, (impl: ImplDef) :: Nil) =>
              Some(impl)
            case _ => None
          }
        case _ => None
      }
    }

    s.findSelectedOfType[ImplDef] orElse getSingleImplDef toRight PreparationError("No class or object selected.")
  }
    
  def perform(selection: Selection, toMove: PreparationResult, parameters: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    val ancestors = ancestorSymbols(toMove).init

    val referencesToMovedClass = index.references(toMove.symbol)

    trace("Selected ImplDef: %s, in package %s, move to %s", toMove.nameString, ancestors map (_.nameString) mkString ("."), parameters)

    def renamePackage(name: String) = {
      val targetPackages = name.split("\\.").toList

      val findFirstPackageToRename = filter {
        case pkg @ PackageDef(_, stats) if stats contains toMove =>
          true
        case pkg @ PackageDef(pid, _) =>
          val packages = ancestorSymbols(pkg) map (_.nameString)
          !(targetPackages startsWith packages)
      }

      val changePackageDeclaration = transform {
        case pkg @ PackageDef(pid, stats) =>

          val surroundingPackages = ancestorSymbols(pkg).init map (_.nameString)

          val newPid = if(targetPackages.startsWith(surroundingPackages)) {
            (targetPackages.drop(surroundingPackages.size))
          } else {
            targetPackages
          }

          if(newPid.isEmpty) {
            toMove
          } else {
            pkg copy (pid = Ident(newPid mkString "."), stats = List(toMove)) replaces pkg
          }
      }

      traverseAndTransformAll(findFirstPackageToRename &> changePackageDeclaration)
    }

    def removeClassFromOldFileAndAddImportToNewIfNecessary(newFullName: String) = {

      val referencesInOriginalFile = referencesToMovedClass.filter(_.pos.source.file == selection.file)

      def hasRelativeReferenceToMovedClass = referencesInOriginalFile.exists {
        // TODO check if the complete qualifier has a range!
        // TODO what if the moved class has a reference to itself?
        case Select(qual, _) if qual.pos.isRange => false
        case t: RefTree => true
      }
            
      val removeClassFromOldFile = replaceTree(toMove, EmptyTree)

      val trans = if(hasRelativeReferenceToMovedClass) {
        removeClassFromOldFile &> addImportTransformation(List(newFullName + "." + toMove.nameString))
      } else {
        removeClassFromOldFile
      }

      transformFile(selection.file, trans)
    }
    
    def adaptDependentFiles(newFullName: String) = {
      val referencesFromOtherFiles = referencesToMovedClass.filterNot(_.pos.source.file == selection.file)

      referencesFromOtherFiles.groupBy(_.pos.source) flatMap {

        case (sourceFile, references) =>

          val alreadyHasImportSelector = references.exists(_.isInstanceOf[ImportSelectorTree])

          def hasReferenceWithoutFullName = references.exists {
            // TODO check if the complete qualifier has a Range!
            case Select(qual, _) if qual.pos.isRange => false
            case t: RefTree => true
          }

          if(!alreadyHasImportSelector && hasReferenceWithoutFullName) {

            val addImport = new AddImportStatement { val global = MoveClass.this.global }

            addImport.addImport(sourceFile.file, newFullName + "." + toMove.nameString)

          } else {

            def hasMovedName(s: ImportSelector) = s.name.toString == toMove.name.toString

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
                    imp copy (expr = Ident(newFullName)) replaces imp
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
                        Import(Ident(newFullName), selector :: Nil))
                  case stmt =>
                    List(stmt)
                }) copyAttrs pkg

              case s @ Select(qualifier, _) if references.contains(s) &&
                  qualifier.pos.isRange /* qualifier is visible in the source code */ =>
                s copy (qualifier = Ident(newFullName)) replaces s

              case ref: Ident if references.contains(ref) && !alreadyHasImportSelector =>
                Ident(newFullName + "." + toMove.name)
            }

            transformFile(sourceFile.file, traverseAndTransformAll(adaptImports))
          }
      }
    }
    
    parameters match {

      case KeepFile(newFullName) =>

        Right(transformFile(selection.file, renamePackage(newFullName)) ++ adaptDependentFiles(newFullName))
        
      case NewFile(newFullName) =>
        
        val insertImports = transform {
          case pkg @ PackageDef(_, stats) if stats contains toMove =>
            val requiredImports = mkImportTrees(neededImports(toMove), newFullName)
            pkg copy (stats = requiredImports ++ stats) replaces pkg
        }
        
        val moveClass = renamePackage(newFullName) &> traverseAndTransformAll(insertImports)

        val newFileChanges = transformFile(selection.file, moveClass) map {
          case Change(file, from, to, src) => new NewFileChange(newFullName, file, from, to, src)
        }

        val oldFileChanges = removeClassFromOldFileAndAddImportToNewIfNecessary(newFullName)

        Right(newFileChanges ++ oldFileChanges ++ adaptDependentFiles(newFullName))
    }
  }

}
