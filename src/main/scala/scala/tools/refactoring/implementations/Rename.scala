/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import scala.Left
import scala.Right
import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.RenameSourceFileChange
import scala.tools.refactoring.common.TextChange

import analysis.TreeAnalysis
import transformation.TreeFactory
import scala.tools.refactoring.common.MoveToDirChange
import scala.reflect.io.AbstractFile

abstract class Rename extends MultiStageRefactoring with MarkOccurrences with TreeAnalysis with analysis.Indexes with TreeFactory with common.InteractiveScalaCompiler {

  import global._

  case class PreparationResult(selectedTree: SymTree, hasLocalScope: Boolean)

  type RefactoringParameters = String

  def prepare(s: Selection) = {

    /*
     * This heuristic decides if this refactoring is local to a single file or not. Note that
     * we could simply query the index for all occurrences of the symbol in question, to see if
     * multiple files are involved or not, but this would require clients to make sure that
     * we always get a fully initialized index.
     */
    def isLocalRename(t: Tree) = {
      def isSelfReference = {
        findParentOfPotentialSelfReference(t, s.root).nonEmpty
      }

      def isLocalSymbol(symbol: Symbol) = {
        def isHiddenOrNoAccessor(symbol: Symbol) = symbol == NoSymbol || symbol.isPrivate

        def hasHiddenOrNoAccessor = {
          if (symbol.isVal || symbol.isVar) {
            def getter = symbol.getter(symbol.owner)
            def setter = symbol.setter(symbol.owner)


            isHiddenOrNoAccessor(getter) && isHiddenOrNoAccessor(setter)
          } else {
            true
          }
        }


        val relatedCtor = s.root.find {
          case dd: DefDef if dd.symbol.isConstructor && !dd.mods.isPrivate && !dd.mods.isPrivateLocal =>
            val relatedParam = dd.vparamss.flatten.find { p =>
              val (p1, p2) = (p.symbol.pos, symbol.pos)
              p1.isDefined && p2.isDefined && p1.point == p2.point
            }

            relatedParam.nonEmpty

          case _ => false
        }

        val isCtorArg = relatedCtor.nonEmpty

        if (isCtorArg) {
          // Better be safe than sorry and assume that constructor arguments might always leak out:
          //  Deciding if constructor arguments might 'leak' out of a file is a non-trivial endeavor,
          //  even if we the know that the class definition is nested in a local block. To do this
          //  properly we would have to examine all supertypes, as well as the return type of the
          //  block, which might be a structural type.
          false
        } else if (symbol.isParameter) {
          val isParamThatMightBeVisibleInOtherFiles = {
            val isNestedInMethodValOrVar = {
              def isMethodValOrVar(s: Symbol) = {
                s.isVal || s.isVar || s.isMethod
              }

              val level = symbol.ownerChain.count(isMethodValOrVar)
              level > 2
            }

            !isNestedInMethodValOrVar
          }

          !isParamThatMightBeVisibleInOtherFiles
        } else {
          symbol.isLocal || (symbol.isPrivate && hasHiddenOrNoAccessor)
        }
      }

      isSelfReference || (t.symbol match {
        case null | NoSymbol => true
        case properSymbol => isLocalSymbol(properSymbol)
      })
    }

    s.selectedSymbolTree match {

      // Has been renamed.. also check for a matching importselector that did the rename
      case Some(t: RefTree) if t.name != t.symbol.name =>
        Right(PreparationResult(t, true))

      case Some(t) =>
        Right(PreparationResult(t, isLocalRename(t)))
      case None => Left(PreparationError("no symbol selected found"))
    }
  }

  def perform(selection: Selection, prepared: PreparationResult, newName: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    trace("Selected tree is %s", prepared.selectedTree)
    val singleTreeSelection = new SingleTreeSelection(prepared.selectedTree, selection.root)

    def generateChanges: List[Change] = {
      val occurences = findOccurrences(singleTreeSelection)

      val textChangesWithTrees = occurences.map { case (occurence, tree)  =>
        (TextChange(occurence.source, occurence.start, occurence.end, newName), tree)
      }

      val renameFileChanges = textChangesWithTrees.flatMap { case (textChange, tree) =>
        if (tree.pos.source.file.name == singleTreeSelection.name + ".scala") {
          Some(RenameSourceFileChange(tree.pos.source.file, newName + ".scala"))
        } else {
          None
        }
      }.distinct

      val moveToDirChanges = {
        mkMoveToDirChanges(textChangesWithTrees.map(_._2), singleTreeSelection.symbol, newName)
      }

      val textChanges = textChangesWithTrees.map(_._1)

      textChanges ::: renameFileChanges ::: moveToDirChanges
    }

    trace(s"Old name is ${singleTreeSelection.name}")
    if (singleTreeSelection.name == newName) Right(Nil)
    else Right(generateChanges)
  }

  /*
   * If we rename a package, it will typically be necessary to move its contents to a new directory.
   * This method generates these changes for all files where the primary package (see below for exact definition)
   * corresponds to the directory the files lies in. We are considering a package to be the primary package of a file,
   * if all non package declarations lie beneath it, and no more deeply nested package satisfies this property. This
   * is best understood by examples:
   *
   * Ex. 1:
   *     package a.b //<-- primary
   *
   *     class C
   *
   * Ex. 2:
   *     package a
   *     package object b { //<-- primary
   *       val x = 42
   *     }
   *
   * Ex. 3:
   *     package a { //<-- primary
   *       package b {
   *         class C
   *       }
   *       class C
   *     }
   */
  private def mkMoveToDirChanges(changedTrees: List[Tree], selectedSymbol: Symbol, newName: String): List[MoveToDirChange] = {
    if (!selectedSymbol.hasPackageFlag) {
      Nil
    } else {
      changedTrees.flatMap { tree =>
        val pkgSymbol = associatedPackageClassSymbol(tree)

        pkgSymbol.flatMap  { pkgSymbol =>
          index.rootOf(tree).flatMap { root =>
            val pkgChain = packageChainReflectedInDirLayout(root)
            val adaptedPkgChain = pkgChain.map {
              case (dir, sym) if sym == pkgSymbol => (newName, pkgSymbol)
              case other => other
            }

            if (adaptedPkgChain == pkgChain) {
              None
            } else {
              val sourceFile = tree.pos.source.file
              val newPath = {
                val parent = parentPath(sourceFile, adaptedPkgChain.size + 1)
                val prefix = if (parent.isEmpty) "" else parent + "/"
                prefix + adaptedPkgChain.map(_._1).mkString("/")
              }

              Some(MoveToDirChange(sourceFile, newPath))
            }
          }
        }
      }
    }
  }

  private def moduleClassOf(symbol: Symbol): Option[Symbol] = symbol match {
    case symbol: ModuleClassSymbol => Some(symbol)
    case symbol: ModuleSymbol => Some(symbol.moduleClass)
    case _ => None
  }

  private def associatedPackageClassSymbol(tree: Tree): Option[Symbol] = {
    tree match {
      case tree: ModuleDef if tree.symbol.isPackageObject => moduleClassOf(tree.symbol.enclosingPackage)
      case tree if tree.symbol != null && tree.symbol.hasPackageFlag => moduleClassOf(tree.symbol)
      case  _ => None
    }
  }

  private def parentPath(source: AbstractFile, levels: Int): String = {
    parentPath(source.path, levels)
  }

  private def parentPath(path: String, levels: Int): String = {
    path.split("/").dropRight(levels).mkString("/")
  }

  private def packageChainReflectedInDirLayout(root: Tree): Seq[(String, Symbol)] = {
    val dirComponents = root.pos.source.file.path.split("/").dropRight(1)
    val pkgChain = primaryPackageChain(root)

    dirComponents.reverse.zip(pkgChain.reverse).takeWhile {
      case (dir, (name, _)) => dir == name
    }.map(_._2).reverse.toSeq
  }

  private def primaryPackageChain(root: Tree): List[(String, Symbol)] = {
    def chainFrom(symbol: Symbol) = {
      symbol.ownerChain.reverse.map(s => (s.nameString, moduleClassOf(s).getOrElse(NoSymbol)))
    }

    root match {
      case pkgDef: PackageDef =>
        val prefix = chainFrom(pkgDef.symbol)

        pkgDef.stats match {
          case stat :: Nil => prefix ::: primaryPackageChain(stat)
          case _ => prefix
        }

      case moduleDef: ModuleDef if moduleDef.symbol.isPackageObject =>
        chainFrom(moduleDef.symbol.enclosingPackage)

      case _ =>
        Nil
    }
  }
}
