/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import analysis.{Indexes, TreeAnalysis}
import transformation.TreeFactory
import common.Change

abstract class ExtractMethod extends MultiStageRefactoring with TreeAnalysis with Indexes with TreeFactory {
  
  import global._
    
  abstract class PreparationResult {
    def selectedMethod: Tree
  }
  
  abstract class RefactoringParameters {
    def methodName: String
  }
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[DefDef] match {
      case Some(defdef) =>
        Right(new PreparationResult {
          val selectedMethod = defdef
        })
      case None => Left(new PreparationError("no enclosing defdef found"))
    }
  }
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    
    import prepared._
    import params._

    val parameters = inboundLocalDependencies(selection, selectedMethod.symbol, index)
    
    val returns = outboundLocalDependencies(selection, selectedMethod.symbol, index)
     
    val newDef = mkDefDef(NoMods, methodName, parameters :: Nil, selection.selectedTopLevelTrees ::: (if(returns.isEmpty) Nil else mkReturn(returns) :: Nil))
    
    val call = mkCallDefDef(methodName, parameters :: Nil, returns)
    
    val extractSingleStatement = selection.selectedTopLevelTrees.size == 1
    
    val findTemplate = filter {
      case Template(_, _, body) => 
        body exists (_ == selectedMethod)
    }
    
    val findMethod = filter {
      case d: DefDef => d == selectedMethod
    }
    
    val replaceBlockOfStatements = topdown {
      matchingChildren {
        transform {
          case block @ BlockExtractor(stats) => {
            mkBlock(stats.replaceSequence(selection.selectedTopLevelTrees, call :: Nil)) setPos block.pos
          }
        }
      }
    }
  
    val replaceExpression = if(extractSingleStatement)
      replaceTree(selection.selectedTopLevelTrees.head, call)
    else
      fail[Tree]
    
    val insertMethodCall = transform {
      case tpl @ Template(_, _, body) => 
        tpl.copy(body = body ::: newDef :: Nil) setPos tpl.pos
    }
    
    val extractMethod = topdown {
      matchingChildren {
        findTemplate &> 
        topdown {
          matchingChildren {
            findMethod &> replaceBlockOfStatements |> replaceExpression
          }
        } &> 
        insertMethodCall
      }
    }
    
    Right(refactor(extractMethod apply abstractFileToTree(selection.file) toList))
  }
}
