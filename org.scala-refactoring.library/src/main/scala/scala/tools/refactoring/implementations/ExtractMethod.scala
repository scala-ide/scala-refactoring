/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import scala.tools.nsc.symtab.Flags

import analysis.Indexes
import analysis.TreeAnalysis
import common.Change
import common.InteractiveScalaCompiler
import transformation.TreeFactory

abstract class ExtractMethod extends MultiStageRefactoring with TreeAnalysis with Indexes with TreeFactory with InteractiveScalaCompiler {

  val global: tools.nsc.interactive.Global
  import global._

  type PreparationResult = Tree

  type RefactoringParameters = String

  def prepare(s: Selection) = {
    s.findSelectedOfType[DefDef] match {
      case _ if s.selectedTopLevelTrees.size == 0 =>
        Left(PreparationError("No expressions or statements selected."))
      case Some(tree) =>
        Right(tree)
      case None =>
        Left(PreparationError("No enclosing method definition found: please select code that's inside a method."))
    }
  }

  def perform(selection: Selection, selectedMethod: PreparationResult, methodName: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    val (call, newDef) = {

      val deps = inboundLocalDependencies(selection, selectedMethod.symbol)

      val parameters = {
        if(deps.isEmpty)
          Nil // no argument list
        else
          deps :: Nil // single argument list with all parameters
      }

      val returns = outboundLocalDependencies(selection, selectedMethod.symbol)

      val returnStatement = if(returns.isEmpty) Nil else mkReturn(returns) :: Nil

      val newDef = mkDefDef(NoMods withPosition (Flags.PRIVATE, NoPosition), methodName, parameters, selection.selectedTopLevelTrees ::: returnStatement)

      val call = mkCallDefDef(methodName, deps :: Nil, returns)

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
          case block @ BlockExtractor(stats) if stats.size > 0 => {
            val newStats = stats.replaceSequence(selection.selectedTopLevelTrees, call :: Nil)
            mkBlock(newStats) replaces block
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

    Right(transformFile(selection.file, extractMethod))
  }
}
