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
     * Tries to remove unneeded imports, but don't 
     * throw them out when it's uncertain whether
     * they are really unneeded. Should be safe when
     * there are compile errors or when used without
     * an interactive compiler.
     */
    val RemoveUnneeded = Value
  }
  
  type Participant = (List[Import] => List[Import])
  
  object CollapseImports extends Participant {
    def apply(trees: List[Import]) = {
      trees.foldRight(Nil: List[Import]) { 
        case (imp: Import, x :: xs) if imp.expr.toString == x.expr.toString => 
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
    def apply(trees: List[Import]) = {
      trees.sortBy {
        case Import(expr, selector :: Nil) if !wildcardImport(selector) => 
          expr.toString + "." + selector.name.toString
        case Import(expr, selectors) => 
          expr.toString
      }
    }
  }

  case class AlwaysUseWildcards(imports: Set[String]) extends Participant {
    def apply(trees: List[Import]) = {
      val seen = collection.mutable.HashSet[String]()
      def asString(t: Tree) = {
        t.filter(_ => true).map {
          case Ident(name) => name.toString
          case Select(_, name) => name.toString
          case _ => ""
        }.reverse.mkString(".")
      }
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
            val newExpr = ↓(setNoPosition) apply duplicateTree(expr) getOrElse expr
            Import(newExpr, neededSelectors)
          } else {
            Import(EmptyTree, Nil)
          }
      }
    }
  }
  
  class FindNeededImports(root: Tree) extends Participant {
    def apply(trees: List[Import]) = {
      
      val rootPackage = root match {
        case root: PackageDef => 
          val rootPackage = topPackageDef(root)
          ancestorSymbolsDesc(rootPackage).map(_.nameString).mkString(".")
        case _ => ""
      }
      
      def importsFromSamePackage(t: Tree) = {
        asSelectorString(t) == rootPackage
      }
      
      neededImports(root) flatMap {
        // warning if binding is never used! and quickfix to replace with `_`!
        case Select(selector, _) if importsFromSamePackage(selector) =>
          None
        case select @ Select(expr, name) =>

          // we don't want to see imports like "java.this.lang..."
          val removeThisTrees = {
            matchingChildren { 
              transform {
                case t: This => 
                  // expand to the full package name
                  val parents = t.symbol.ownerChain.takeWhile(_.nameString != nme.ROOT.toString).reverse
                  Ident(parents map (_.nameString) mkString ".")
              }
            }
          }
            
          // copy the tree and delete all positions so the full path will be written
          val newExpr = ↓(setNoPosition &> removeThisTrees) apply duplicateTree(expr) getOrElse expr
          val typeName = select.symbol.nameString
          Some(Import(newExpr, List(new ImportSelector(if(typeName == name.toString) name else typeName, -1, name, -1))))
      }
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
        new FindNeededImports(selection.root) :: SortImports :: Nil
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
