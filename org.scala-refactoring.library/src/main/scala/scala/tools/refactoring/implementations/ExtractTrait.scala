package scala.tools.refactoring
package implementations

import scala.tools.nsc.symtab.Flags
import scala.tools.refactoring.MultiStageRefactoring

import common.Change

/**
 * Extracts members (vals, vars and defs) from a trait/class/object
 * into a new trait.
 */
abstract class ExtractTrait extends MultiStageRefactoring with common.InteractiveScalaCompiler with analysis.Indexes with ImportsHelper {

  import global._

  case class PreparationResult(classDef: ClassDef, extractableMembers: List[ValOrDefDef])

  case class RefactoringParameters(traitName: String, defFilter: ValOrDefDef => Boolean)

  override def prepare(s: Selection) = {
    def isDefDefExtractable(defdef: DefDef) = {
      defdef.hasSymbol && !(defdef.symbol.isPrivate || defdef.symbol.isConstructor) &&
      !defdef.mods.hasFlag(Flags.ACCESSOR) && !defdef.mods.isParamAccessor
    }

    def isValDefExtractable(valdef: ValDef) = {
      valdef.hasSymbol && !valdef.mods.isParamAccessor
    }

    s.findSelectedOfType[ClassDef] match {
      case None => Left(PreparationError("no class def selected"))
      case Some(classDef) => {
        val accessors = classDef.impl.body.collect { case defdef: DefDef if defdef.mods.hasFlag(Flags.ACCESSOR) && !defdef.mods.isPrivate => defdef } map (_.nameString)
        val extractableMembers = classDef.impl.body.collect {
          case defdef: DefDef if isDefDefExtractable(defdef) => defdef
          case valdef: ValDef if isValDefExtractable(valdef) && (accessors contains valdef.nameString) => valdef
        }
        Right(PreparationResult(classDef, extractableMembers))
      }
    }
  }

  override def perform(selection: Selection, prep: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    def getsExtracted(defFilter: ValOrDefDef => Boolean)(tree: Tree) = tree match {
      case valOrDefDef: ValOrDefDef if !valOrDefDef.mods.isParamAccessor => defFilter(valOrDefDef)
      case _ => false
    }
    val extracted = prep.classDef.impl.body filter getsExtracted(params.defFilter)
    val extractedVariables = extracted collect { case valdef: ValDef => valdef }
    val extractedMethods = extracted collect { case defdef: DefDef => defdef }

    val extractedVariablesNames = extractedVariables.map(_.nameString)

    val (traitBody, classBody) = prep.classDef.impl.body.partition {
      case valdef: ValDef => extractedVariables contains valdef
      case defdef: DefDef if defdef.mods.hasFlag(Flags.ACCESSOR) => extractedVariablesNames contains defdef.nameString.stripSuffix("_=")
      case defdef: DefDef => extractedMethods contains defdef
      case _ => false
    }

    val classDef = prep.classDef

    def symbols(trees: List[Tree]) = trees collect { case t: Tree if t.hasSymbol => t.symbol}

    val classMembers = symbols(classBody)
    val traitMembers = symbols(traitBody)
    val arePrivateClassMembersAccessed = areSymbolsAccessed(traitBody, classMembers.filter(_.isPrivate))
    val arePrivateTraitMembersAccessed = areSymbolsAccessed(classBody, traitMembers.filter(_.isPrivate))
    val isExtractionWellDefined = !arePrivateClassMembersAccessed && !arePrivateTraitMembersAccessed
    if(!isExtractionWellDefined) {
      return Left(RefactoringError("can't reference private class members from extracted trait or private trait members from class"))
    }

    val enclosingPackages = findEnclosingPackages(selection.root, classDef.pos)
    val enclosing = enclosingPackages.headOption getOrElse selection.root

    val sourceFileChanges = refactorClass(classBody, enclosing, params.traitName, classDef)

    val traitNeedsSelfType = areSymbolsAccessed(traitBody, classMembers.filter(!_.isPrivate))
    val traitFileChanges = refactorTrait(traitBody, traitNeedsSelfType, enclosing, enclosingPackages, params.traitName, classDef)

    Right(sourceFileChanges:::traitFileChanges)
  }

  private def refactorClass(classBody: List[Tree], enclosing: Tree, traitName: String, classDef: ClassDef) = {
    val addTrait = {
      val extractedTrait = mkIdent(traitName, classDef.tparams)
      transform {
        case t @ Template(parents, self, body) => {
          val impl = Template(parents:::List(extractedTrait), self, classBody)
          impl replaces t
        }
      }
    }

    val templateFilter = filter {
      case classDef.impl => true
    }

    val classRefactoring = topdown {
      matchingChildren {
        templateFilter &> addTrait
      }
    } &> addRequiredImports(None, None)

    refactor(classRefactoring(enclosing/*enclosingPackages.head*/).toList)
  }

  private def refactorTrait(
      traitBody: List[Tree],
      needsSelfTypeAnnotation: Boolean,
      enclosing: Tree,
      enclosingPackages: List[PackageDef],
      traitName: String,
      classDef: ClassDef) = {
    val traitRefactoring = {
      transform {
        case pkg: PackageDef => {
          val imports = pkg.stats.collect {case importTree: Import => importTree.copy()}
          val selfType = if(needsSelfTypeAnnotation) {
            val classIdent = mkIdent(classDef.nameString, classDef.tparams)
            ValDef(Modifiers(Flags.SYNTHETIC), newTermName("this"), CompoundTypeTree(Template(List(classIdent), emptyValDef, Nil)), EmptyTree)
          } else {
            emptyValDef
          }
          // remove `override` modifiers
          val preparedTraitBody = traitBody.map {
            case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if mods.hasFlag(Flags.OVERRIDE) => {
              val withoutOverride = mods &~ Flags.OVERRIDE
              DefDef(withoutOverride, name, tparams, vparamss, tpt, rhs) replaces d
            }
            case t@_ => t
          }
          val impl = Template(Nil, selfType, preparedTraitBody)
          val mods = Modifiers(Flags.TRAIT).withPosition(Flags.TRAIT, NoPosition)
          val extractedTrait = ClassDef(mods, newTypeName(traitName), classDef.tparams, impl)
          val flattenedPkg = PackageDef(flattenPkgPids(enclosingPackages), imports:::List(extractedTrait))
          flattenedPkg replaces pkg
        }
      } &> addRequiredImports(None, None)
    }

    refactor(traitRefactoring(enclosing).toList).map(_.toNewFile(""))
  }

  private def areSymbolsAccessed(accessors: List[Tree], targets: List[Symbol]) = {
    def collectReferences(trees: List[Tree], symbols: List[Symbol]) = {
      def symbolsFilter(symbols: List[Symbol])(tree: Tree): Boolean = tree match {
        case tree: Tree if tree.hasSymbol => symbols contains tree.symbol
        case _ => false
      }
      val symbolsTraverser = new FilterTreeTraverser(symbolsFilter(symbols))
      symbolsTraverser.traverseTrees(trees)
      val hits = symbolsTraverser.hits.toList
      hits
    }

    val references = collectReferences(accessors, targets)

    !references.isEmpty
  }

  private def flattenPkgPids(packages: List[PackageDef]) = {

    def flattenPkgPidsHelper(inner: Tree, outer: Tree): Select = inner match {
      case Ident(name) => Select(outer, name)
      case Select(qualifier, name) => Select(flattenPkgPidsHelper(qualifier, outer), name)
      case _ => throw new Exception("unknown pid tree")
    }

    val flattenedPkgPid = packages match {
      case Nil => Ident("")
      case pkg :: pkgs =>
        // Copy the tree so it doesn't drag layout from the original file to the new one.
        val newPid = shallowDuplicate(pkg.pid) setPos NoPosition
        pkgs.foldLeft(newPid: RefTree)((outer, innerPkg) => flattenPkgPidsHelper(innerPkg.pid, outer))
    }

    flattenedPkgPid
  }

  private def findEnclosingPackages(root: Tree, pos: Position) = root collect {
    case pkg @ PackageDef(pid, stats) if pkg.pos.properlyIncludes(pos) => pkg
  }

  private def mkIdent(name: String, typeParams: List[TypeDef]) = {
    val tparams = typeParams match {
      case Nil => ""
      case ts => "[" + ts.map(_.nameString).mkString(", ") + "]"
    }
    Ident(newTermName(name + tparams))
  }

}
