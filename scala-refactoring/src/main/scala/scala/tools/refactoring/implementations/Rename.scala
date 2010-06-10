/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package implementations

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import common.Change

abstract class Rename extends MultiStageRefactoring with analysis.TreeAnalysis with analysis.Indexes {
  
  import global._
    
  case class PreparationResult(selectedLocal: SymTree, hasLocalScope: Boolean)
  
  abstract class RefactoringParameters {
    def newName: String
  }
  
  def prepare(s: Selection) = {
    s.selectedSymbolTree match {
      case Some(t) =>
        Right(PreparationResult(t, s.findSelectedOfType[DefDef] map (_ != t) getOrElse false))
      case None => Left(PreparationError("no symbol selected found"))
    }
  }
  
  override def refactor(changed: List[global.Tree]): List[Change] = context("main") {
    createChanges(changed) toList
  }
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Tree]] = {

    import params._
    
    trace("Selected tree is %s", prepared.selectedLocal)
    
    val occurences = (prepared.selectedLocal :: index.occurences(prepared.selectedLocal.symbol)) distinct 
    
    occurences foreach (s => trace("Symbol is referenced at %s (%s:%s)", s, s.pos.source.file.name, s.pos.line))
    
    val canRename = filter {
      case t: Tree => occurences contains t 
    }
    
    val renameTree = transform {
      case t @ ImportSelectorTree(name, rename) =>  ImportSelectorTree(NameTree(newName) setPos name.pos, rename) setPos t.pos
      case s: SymTree => mkRenamedSymTree(s, newName)
      case t: TypeTree => 
      
        val newType = t.tpe map {
          case r @ RefinedType(parents, _) =>
            r.copy(parents = parents map {
              case TypeRef(_, sym, _) if sym == prepared.selectedLocal.symbol =>
                new Type {
                  override def safeToString: String = newName
                }
              case t => t 
            })
          case t => t
        }
      
        val typeTree = new TypeTree
        typeTree setType newType
        typeTree setPos t.pos
    }
    
    val rename = ↓(canRename &> renameTree |> id)
    
    val renamed = occurences flatMap rename.apply
    
    Right(renamed)
  }
}
