/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change

abstract class OrganizeImports extends MultiStageRefactoring {
  
  import global._
  
  class PreparationResult(val missingTypes: List[String] = Nil)
  
  /**
   * Imports that should be added are passed as tuples in the form
   * ("package.declaration", "TypeName")
   */
  class RefactoringParameters(val importsToAdd: List[(String, String)] = Nil)
  
  def prepare(s: Selection): Either[PreparationError, PreparationResult] = {
    
    def getMissingTypeNameForErroneousTree(t: Tree): String = t match {
      case Apply(Select(n: New, _), args) => 
        n.tpt.nameString
      case Apply(fun, args) => 
        fun.nameString
      case t: Select => 
        t.name.toString
      case t: Ident => 
        t.name.toString
      case t => 
        val n = t.nameString
        n
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
    lazy val dependencies = unit.depends map (_.name.toString)
    lazy val dependentPackageObjectNames = unit.depends filter (_.isPackageObjectClass) map (_.tpe.safeToString)
    
    val newImports = params.importsToAdd.map {
      case (pkg, tpe) =>
        new Import(Ident(pkg), new ImportSelector(tpe, -1, tpe, -1) :: Nil)
    }
            
    val organizeImports = transform {
      // in nested packages, skip the outer packages
      case p @ PackageDef(_, stats @ (NoPackageDef(_) :: _)) =>
         
        p copy (stats = stats partition {
            case _: Import => true
            case _ => false
          } match {
            case (existingImports, others) => 
            
              val sortImports: List[Tree] => List[Tree] = _.sortBy {
                case t: Import => t.expr.toString
              }
                
              val collapseImports: List[Tree] => List[Tree] = _.foldRight(Nil: List[Import]) { 
                case (imp: Import, x :: xs) if imp.expr.toString == x.expr.toString => 
                  x.copy(selectors = x.selectors ::: imp.selectors).setPos(x.pos) :: xs
                case (imp: Import, xs) => 
                  imp :: xs
              }

              def importsAll(i: ImportSelector) = i.name == nme.WILDCARD
              
              val simplifyWildcards: List[Tree] => List[Tree] = {
                def renames(i: ImportSelector) = i.name != i.rename
                
                _ map {
                  case imp @ Import(_, selectors) if selectors.exists(importsAll) && !selectors.exists(renames) => 
                    imp.copy(selectors = selectors.filter(importsAll)).setPos(imp.pos)
                  case imp =>
                    imp
                }
              }
              
              val removeUnused: List[Tree] => List[Tree] = {
                
                def importSelectorImportsFromNeededPackageObject(t: Tree) = {
                  dependentPackageObjectNames.exists {
                    name =>                       
                      val treeString = createText(t)
                      name == "object " + treeString + "package"
                  }                  
                }
                
                val additionallyImportedTypes = params.importsToAdd.unzip._2
                
                _ map {
                  case imp @ Import(expr, selectors) =>
                    val neededSelectors = selectors.filter {
                      s => importsAll(s) || 
                           dependencies.contains(s.name.toString) || 
                           additionallyImportedTypes.contains(s.name.toString) ||
                           importSelectorImportsFromNeededPackageObject(expr)
                    }
                    
                    if(neededSelectors.size > 0) {
                      val newExpr = (↓(setNoPosition) apply duplicateTree(expr) getOrElse expr)
                      val newImport = imp.copy(selectors = neededSelectors, expr = newExpr)
                      
                      newImport
                    }
                    else EmptyTree
                }
              }
              
              ((sortImports andThen collapseImports andThen simplifyWildcards andThen removeUnused) apply (newImports ::: existingImports)) ::: others
          }
        ) setPos p.pos
    }
    
    val changes = (organizeImports |> topdown(matchingChildren(organizeImports))) apply abstractFileToTree(selection.file)

    Right(refactor(changes toList))
  }
}
