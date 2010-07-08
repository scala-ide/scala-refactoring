/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import tools.nsc.symtab.Flags
import analysis.{Indexes, TreeAnalysis}
import transformation.TreeFactory
import common.Change

abstract class ExtractMethod extends MultiStageRefactoring with TreeAnalysis with Indexes with TreeFactory {
  
  import global._
    
  type PreparationResult = Tree
  
  type RefactoringParameters = String

  def prepare(s: Selection) = {
    s.findSelectedOfType[DefDef] match {
      case Some(defdef) => Right(defdef)
      case None => Left(new PreparationError("no enclosing defdef found"))
    }
  }
    
  def perform(selection: Selection, selectedMethod: PreparationResult, methodName: RefactoringParameters): Either[RefactoringError, List[Change]] = {
        
    val (call, newDef) = {

      val parameters = inboundLocalDependencies(selection, selectedMethod.symbol, index)
      
      val returns = outboundLocalDependencies(selection, selectedMethod.symbol, index)
      
      val returnStatement = if(returns.isEmpty) Nil else mkReturn(returns) :: Nil
      
      val newDef = mkDefDef(NoMods withPosition (Flags.PRIVATE, NoPosition), methodName, parameters :: Nil, selection.selectedTopLevelTrees ::: returnStatement)
      
      val call = mkCallDefDef(methodName, parameters :: Nil, returns)
         
      (call, newDef)
    }
    
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
            mkBlock(stats.replaceSequence(selection.selectedTopLevelTrees, call :: Nil)) replaces block
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
        tpl copy(body = body ::: newDef :: Nil) replaces tpl
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
