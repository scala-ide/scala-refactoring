/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.implementations

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.analysis.FullIndexes
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change

abstract class Rename extends MultiStageRefactoring {
  
  import global._
  
  abstract class PreparationResult {
    def selectedLocal: SymTree
  }
  
  abstract class RefactoringParameters {
    def newName: String
  }
  
  def prepare(s: Selection) = {
    s.selectedSymbolTree match {
      case Some(t) =>
        Right(new PreparationResult {
          val selectedLocal = t
        })
      case None => Left(PreparationError("no symbol selected found"))
    }
  }
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, TreeModifications] = {

    trace("Selected tree is %s", prepared.selectedLocal)
    
    index.occurences(prepared.selectedLocal.symbol) foreach (s => trace("Symbol is referenced at %s (line %s)", s, s.pos.line))

    val changes = new ModificationCollector {
      index occurences (prepared.selectedLocal.symbol) foreach {
        transform(_) {
          case s: SymTree if s.symbol == prepared.selectedLocal.symbol => mkRenamedSymTree(s, params.newName)
        }
      }
    }
    
    Right(changes)
  }
}
