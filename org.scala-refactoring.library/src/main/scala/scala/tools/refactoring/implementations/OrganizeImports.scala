/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.{TreeTraverser, Change}
import transformation.TreeFactory
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap

/**
 * A refactoring that recomputes and reorganizes import statements in a file.
 * 
 * 
 */
abstract class OrganizeImports extends MultiStageRefactoring with TreeFactory with TreeTraverser with UnusedImportsFinder with analysis.CompilationUnitDependencies with common.InteractiveScalaCompiler with common.TreeExtractors {
  
  import global._
  
  class PreparationResult(val missingTypes: List[String] = Nil)
  
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
  
  trait Participant extends (List[Import] => List[Import]) {
    def importAsString(t: Tree) = {
      val ancestorSyms = ancestorSymbols(t)
      ancestorSyms map (_.nameString) filterNot (_ == "package") mkString  (".")
    }
    def stripPositions(t: Tree) = {
      topdown(setNoPosition) apply t.duplicate getOrElse t
    }
  }
  
  object CollapseImports extends Participant {
    def apply(trees: List[Import]) = {
      trees.foldRight(Nil: List[Import]) { 
        case (imp: Import, x :: xs) if createText(imp.expr) == createText(x.expr) => 
          x.copy(selectors = x.selectors ::: imp.selectors).setPos(x.pos) :: xs
        case (imp: Import, xs) => 
          imp :: xs
      }
    }
  }
  
  object ExpandImports extends Participant {
    def apply(trees: List[Import]) = {
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
    def apply(trees: List[Import]) = {
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
    
    def apply(trees: List[Import]) = {
      trees.sortBy {
        case i @ Import(expr, selector :: Nil) if !wildcardImport(selector) => 
          asText(expr) + "." + selector.name.toString
        case i @ Import(expr, selectors) => 
          asText(expr)
      }
    }
  }

  case class AlwaysUseWildcards(imports: Set[String]) extends Participant {
    def apply(trees: List[Import]) = {
      val seen = collection.mutable.HashSet[String]()
      trees flatMap {
        case imp @ Import(qual, selectors) if imports.contains(asString(qual)) && !selectors.exists(renames) =>
          if(seen.contains(asString(qual))) {
            None
          } else {
            seen += asString(qual)
            Some(Import(qual, List(ImportSelector(nme.WILDCARD, -1, nme.WILDCARD, -1))).copyAttrs(imp))
          }
        case t => Some(t)
      }
    }
  }
  
  case class GroupImports(groups: List[String]) extends Participant {
    def apply(trees: List[Import]) = {
      
      val grouped = new LinkedHashMap[String, ListBuffer[Import]] {
        groups.foreach(this += _ → new ListBuffer[Import])
      }
      val ungrouped: ListBuffer[Import] = new ListBuffer[Import]

      trees foreach { imp =>
        
        val inserts = grouped flatMap {
          case (key, map) if imp.expr.toString.startsWith(key +".") || imp.expr.toString == key => 
            map += imp
            Some(key)
          case _ => None
        }
        
        if(inserts.isEmpty) ungrouped += imp
      }
      
      val spacer = Import(SourceLayoutTree(SourceLayouts.Newline), Nil)
      
      val allImports = (grouped.values.toList.map(_.toList) ::: List(ungrouped.toList)).filterNot(_.isEmpty)
      
      if(allImports.size > 1) {
        allImports.reduceLeft ((l1: List[Import], l2: List[Import]) => l1 ++ List(spacer) ++ l2)
      } else {
        allImports.flatten
      }
    }
  }
  
  object SortImportSelectors extends Participant {
    def apply(trees: List[Import]) = {
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
    
    def apply(trees: List[Import]) = {
      
      val importsNames = allNeededImports map importAsString
        
      val result = trees flatMap {
        case imp @ Import(expr, selectors) =>
          val pkgName = importAsString(expr) +"."
          
          val neededSelectors = selectors.filter { selector =>
            selector.name == nme.WILDCARD || importsNames.contains(pkgName + selector.name)
          }
          
          // If parts of the expr aren't ranges, then we have an import that depends on an
          // other import (see OrganizeImportsRecomputeAndModifyTest#importDependingOnImport)
          val exprIsAllRangePos = {
            // no Tree#forall, so we use double-negative
            !expr.exists(t => !t.pos.isRange)
          }
          
          val invisiblePartIsDefaultImported = {
            findDeepestNeededSelect(expr) exists isQualifierDefaultImported
          }
          
          if(neededSelectors.size == selectors.size && (exprIsAllRangePos || invisiblePartIsDefaultImported)) {
            Some(imp)
          } else if(neededSelectors.size > 0) {
            Some(Import(Ident(importAsString(expr)), neededSelectors))
          } else {
            None
          }
      }
      
      // remove duplicates:
      result.foldLeft(Nil: List[Import]) {
        case (rest, imp) if rest.exists(t => t.toString == imp.toString) =>
          rest 
        case (rest, imp) => imp :: rest
      } reverse
    }
  }
  
  class RemoveUnused(unit: RichCompilationUnit, importsToAdd: List[(String, String)]) extends Participant {
    def apply(trees: List[Import]) = {
      val additionallyImportedTypes = importsToAdd.unzip._2
      trees map {
        case imp @ Import(expr, selectors) =>
          
          val neededSelectors = selectors.filter { s =>
            neededImportSelector(unit, expr, s) ||
            additionallyImportedTypes.contains(s.name.toString)                 
          }
          
          if(neededSelectors.size > 0) {
            Import(stripPositions(expr), neededSelectors)
          } else {
            Import(EmptyTree, Nil)
          }
      }
    }
  }
  
  class FindNeededImports(root: Tree, enclosingPackage: String) extends Participant {
    def apply(trees: List[Import]) = {
      mkImportTrees(neededImports(root), enclosingPackage)
    }
  }
  
  class AddNewImports(importsToAdd: List[(String, String)]) extends Participant {
    def apply(trees: List[Import]) = {
      val newImports = importsToAdd map (mkImportFromStrings _).tupled
      newImports ::: trees
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
      case _ => "Unhandled tree: "+ t.getClass.getSimpleName +". You found a bug! Please report it."
    }
    
    val erroneousTrees = s.root.filter {
      // TypeTrees are not particularly useful on their own, so try to find a better one
      case _: TypeTree => false
      case t: Tree if t.tpe != null && t.tpe.isError => true
      case _ => false
    }
    
    val missingImportNames = erroneousTrees map getMissingTypeNameForErroneousTree toList
    
    Right(new PreparationResult(missingImportNames))
  }
  
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    
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
        new RecomputeAndModifyUnused(neededImports(selection.root)) :: Nil
        
      case Dependencies.RemoveUnneeded =>   
        val unit = compilationUnitOfFile(selection.pos.source.file).get
        new AddNewImports(params.importsToAdd) :: SortImports :: new RemoveUnused(unit, params.importsToAdd) :: Nil  
    }
    
    val participants = importStrategy ::: params.options

    val organizeImports = locatePackageLevelImports &> transformation[(PackageDef, List[Import], List[Tree]), Tree] {
      case (p, existingImports, others) =>
        val imports = scala.Function.chain(participants)(existingImports)         
        p copy (stats = imports ::: others) replaces p
    }

    Right(transformFile(selection.file, organizeImports |> topdown(matchingChildren(organizeImports))))
  }
}
