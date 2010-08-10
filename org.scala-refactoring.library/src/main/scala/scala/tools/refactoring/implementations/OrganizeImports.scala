/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change

abstract class OrganizeImports extends MultiStageRefactoring {
  
  import global._
  
  class PreparationResult
  
  class RefactoringParameters
  
  def prepare(s: Selection): Either[PreparationError, PreparationResult] = Right(new PreparationResult)
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    
    val unit = global.unitOfFile(selection.pos.source.file)
    val dependencies = unit.depends map (_.name.toString)

    
    //find all types that are used in the CU.
    
    val organizeImports = transform {
       case p @ PackageDef(_, stats) =>
        
          p copy (stats = stats partition {
              case _: Import => true
              case _ => false
            } match {
              case (imports, others) => 
              
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
                  _ map {
                    case imp @ Import(_, selectors) =>
                      val neededSelectors = selectors.filter(s => importsAll(s) || dependencies.contains(s.name.toString))
                      if(neededSelectors.size > 0)
                        imp.copy(selectors = neededSelectors).setPos(imp.pos)
                      else EmptyTree
                  }
                }
                
                ((sortImports andThen collapseImports andThen simplifyWildcards andThen removeUnused) apply imports) ::: others
            }
          ) setPos p.pos
    }
    
    val changes = organizeImports apply abstractFileToTree(selection.file)

    Right(refactor(changes toList))
  }
}
