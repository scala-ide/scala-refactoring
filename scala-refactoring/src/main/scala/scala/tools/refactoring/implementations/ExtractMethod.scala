/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package implementations

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import common.Change
import analysis.FullIndexes
import sourcegen.Transformations

object IntepreterFix {
  import scala.tools.nsc._
  import Interpreter._

  def break(args: DebugParam[_]*): Unit = {
    val intLoop = new InterpreterLoop
    intLoop.settings = {
      val s = new Settings(Console.println)
      // need to pass this explicitly to the settings for Scalac.
      // See: http://old.nabble.com/-scala--recent-changes-in-2.8-nightly-classpath-management-td26233977.html
      s.classpath.value = System.getProperty("java.class.path")
      s
    }
    intLoop.createInterpreter
    intLoop.in = interpreter.InteractiveReader.createDefault(intLoop.interpreter)

    // rebind exit so people don't accidentally call System.exit by way of predef
    intLoop.interpreter.beQuietDuring {
      intLoop.interpreter.interpret("""def exit = println("Type :quit to resume program execution.")""")
      for (p <- args) {
        
        println("binding type: "+ p.manifest)
        intLoop.interpreter.bind(p.name, p.typeStr, p.param)
        println("%s: %s".format(p.name, p.typeStr))
      }
    }
    intLoop.repl()
    intLoop.closeInterpreter
  }
}

import scala.tools.nsc.Interpreter.DebugParam
import IntepreterFix._

abstract class ExtractMethod extends MultiStageRefactoring {
  
  import global._
  import Transformations._
  
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
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Tree]] = {
    
    import prepared._
    import params._

    val parameters = inboundLocalDependencies(selection, selectedMethod.symbol, index)
    
    val returns = outboundLocalDependencies(selection, selectedMethod.symbol, index)
     
    val newDef = mkDefDef(NoMods, methodName, parameters :: Nil, selection.selectedTopLevelTrees ::: (if(returns.isEmpty) Nil else mkReturn(returns) :: Nil))
    
    val call = mkCallDefDef(NoMods, methodName, parameters :: Nil, returns)
    
    val findTemplate = predicate[Tree] {
      case Template(_, _, body) => 
        body exists (_ == selectedMethod) //sameTree?
    }
    
    val findMethod = predicate[Tree] {
      case d: DefDef => d == selectedMethod
    }
    
    val replaceBlockOfStatements = Transformations.transform[Tree, Tree] {
      case block: Block => {
        mkBlock(replace(block, selection.selectedTopLevelTrees, call :: Nil)) setPos block.pos
      }
    }
    
    val replaceExpression = Transformations.transform[Tree, Tree] {
      case t: Tree if selection.selectedTopLevelTrees.size == 1 && t == selection.selectedTopLevelTrees.head => 
        call //setPos t.pos
    }
    
    val insertMethodCall = Transformations.transform[Tree, Tree] {
      case tpl @ Template(_, _, body) => 
        tpl.copy(body = body ::: newDef :: Nil) setPos tpl.pos
    }
    
    val extractMethod = ↓(any(findTemplate &> ↓(any(findMethod &> ↓(any(replaceBlockOfStatements)) |> ↓(any(replaceExpression))))  &> insertMethodCall ))
        
    Right(extractMethod apply abstractFileToTree(selection.file) toList)
  }
}
