/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory

abstract class OrganizeImports extends MultiStageRefactoring with TreeFactory with common.TreeTraverser {
  
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
    val dependentPackageObjectNames = unit.depends filter (_.isPackageObjectClass) map (_.tpe.safeToString)
    val dependentModules = {  
      
      def isType(t: Tree) = 
        t.tpe != null && t.tpe != NoType && t.tpe.typeSymbol != NoSymbol
      
      // a Traverser that ignores imports 
      val importsIgnoringTraverser = new FilterTreeTraverser(isType) {
        override def traverse(tree: Tree): Unit = tree match { 
          case Import(_, _) =>  
          case _ => super.traverse(tree); 
        }
      }
      
      // we also need all the dependencies of the compilation unit
      val unitDependencies = unit.depends filterNot (s => s.isModuleClass || s == NoSymbol) toList
    
      (unitDependencies ::: (filterTree(unit.body)(importsIgnoringTraverser) map (_.tpe.typeSymbol))) distinct
    }
    
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

        def wildcardImport(i: ImportSelector) = i.name == nme.WILDCARD
        
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
          
          def importSelectorImportsFromNeededPackageObject(t: Tree) = {
            dependentPackageObjectNames.exists {
              name =>                       
                val treeString = createText(t)
                name == "object " + treeString + "package"
            }                  
          }
          
          def isWildcardImportNeeded(expr: Tree, s: ImportSelector) = {
            
            def isDependentModule(m: Symbol) = {
              val moduleName = if (m.isModuleClass) m.fullName else m.owner.fullName                          
              val importName = expr.symbol.fullName                          
              moduleName == importName
            }
            
            expr.symbol == NoSymbol || dependentModules.exists(isDependentModule)
          }
          
          val additionallyImportedTypes = params.importsToAdd.unzip._2
          
          _ map {
            case imp @ Import(expr, selectors) =>
              val neededSelectors = selectors.filter { s =>
                (wildcardImport(s) && isWildcardImportNeeded(expr, s)) ||
                dependentModules.exists(m => m.name.toString == s.name.toString) ||
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
        
        val newImports = params.importsToAdd map (mkImportFromStrings _).tupled

        p copy (stats = ((sortImports andThen collapseImports andThen simplifyWildcards andThen removeUnused) apply (newImports ::: existingImports)) ::: others) replaces p
    }

    val changes = (organizeImports |> topdown(matchingChildren(organizeImports))) apply abstractFileToTree(selection.file)

    Right(refactor(changes toList))
  }
}
