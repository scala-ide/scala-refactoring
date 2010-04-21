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
    
    val occurences = index.occurences(prepared.selectedLocal.symbol) 
    
    occurences foreach (s => trace("Symbol is referenced at %s (%s:%s)", s, s.pos.source.file.name, s.pos.line))

    val changes = new ModificationCollector {
      occurences foreach {
        transform2(_) {
          case t: Tree => occurences contains t
        } {
          case s: SymTree => mkRenamedSymTree(s, params.newName)
          case t: TypeTree => 
          
            val newType = t.tpe map {
              case r @ RefinedType(_ :: parents, _) =>
                r.copy(parents = parents map {
                  case TypeRef(_, sym, _) if sym == prepared.selectedLocal.symbol =>
                    NamedType(params.newName, null)
                  case t => t 
                })
              case t => t
            }
          
            new TypeTree {
              tpe = newType
              pos = t.pos
            }
        }
      }
    }
    
    Right(changes)
  }
}
