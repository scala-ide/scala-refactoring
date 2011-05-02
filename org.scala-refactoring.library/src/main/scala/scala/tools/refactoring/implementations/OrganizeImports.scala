/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations
import common.{TreeTraverser, Change}
import transformation.TreeFactory

abstract class OrganizeImports extends MultiStageRefactoring with TreeFactory with TreeTraverser with UnusedImportsFinder with common.InteractiveScalaCompiler {
  
  import global._
  
  class PreparationResult(val missingTypes: List[String] = Nil)
  
  /**
   * Imports that should be added are passed as tuples in the form
   * ("package.declaration", "TypeName")
   */
  class RefactoringParameters(val importsToAdd: List[(String, String)] = Nil)
  
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
      case _ => "Unhandled tree: "+ t.getClass.getSimpleName +". Please report a bug."
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
    
    val unit = compilationUnitOfFile(selection.pos.source.file).get
    
    val organizeImports = locatePackageLevelImports &> transformation[(PackageDef, List[Import], List[Tree]), Tree] {
      case (p, existingImports, others) =>
        val sortImports: List[Tree] => List[Tree] = _.sortBy {
          case t: Import => t.expr.toString
        }
          
        val collapseImports: List[Tree] => List[Tree] = _.foldRight(Nil: List[Import]) { 
          case (imp: Import, x :: xs) if imp.expr.toString == x.expr.toString => 
            x.copy(selectors = x.selectors ::: imp.selectors).setPos(x.pos) :: xs
          case (imp: Import, xs) => 
            imp :: xs
        }
        
        val simplifyWildcards: List[Tree] => List[Tree] = {
          def renames(i: ImportSelector) = i.name != i.rename
          
          _ map {
            case imp @ Import(_, selectors) if selectors.exists(wildcardImport) && !selectors.exists(renames) => 
              imp.copy(selectors = selectors.filter(wildcardImport)).setPos(imp.pos)
            case imp =>
              imp
          }
        }
        
        val removeUnused: List[Tree] => List[Tree] = {
          
          val additionallyImportedTypes = params.importsToAdd.unzip._2
          
          _ map {
            case imp @ Import(expr, selectors) =>
              
              val neededSelectors = selectors.filter { s =>
                neededImportSelector(unit, expr, s) ||
                additionallyImportedTypes.contains(s.name.toString)                 
              }
              
              if(neededSelectors.size > 0) {
                val newExpr = ↓(setNoPosition) apply duplicateTree(expr) getOrElse expr
                
                Import(newExpr, neededSelectors)
              } else {
                EmptyTree
              }
          }
        }
        
        val newImports = params.importsToAdd map (mkImportFromStrings _).tupled

        p copy (stats = ((sortImports andThen collapseImports andThen simplifyWildcards andThen removeUnused) apply (newImports ::: existingImports)) ::: others) replaces p
    }

    Right(transformFile(selection.file, organizeImports |> topdown(matchingChildren(organizeImports))))
  }
}
