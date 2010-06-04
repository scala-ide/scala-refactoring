/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package implementations

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.analysis.FullIndexes

abstract class ExtractLocal extends MultiStageRefactoring {
  
  import global._
  
  abstract class PreparationResult {
    def selectedExpression: Tree
  }
  
  abstract class RefactoringParameters {
    def name: String
  }
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[TermTree] match {
      case Some(term) =>
        Right(new PreparationResult {
          val selectedExpression = term
        })
      case None => Left(new PreparationError("no term selected"))
    }
  }
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Tree]] = {
    
    import prepared._
    import params._
    
    implicit def replacesTree(t1: Tree) = new {
      def replaces(t2: Tree) = t1 setPos t2.pos
    }
        
    trace("Selected: %s", selectedExpression)
    
    val newVal = mkValDef(name, selectedExpression)
    val valRef = Ident(name)
    
    def findBlockInsertionPosition(root: Tree, near: Tree) = {
      
      def isCandidateForInsertion(t: Tree) = t.pos.includes(near.pos) && PartialFunction.cond(t) {
        case If(_, thenp, _    ) if thenp.pos.includes(near.pos) => true
        case If(_, _    , elsep) if elsep.pos.includes(near.pos) => true
        case b: Block => true
      }
      
      val insertionPoint = root.find {
        case t: Tree if isCandidateForInsertion(t) =>
          // find the smallest possible candidate
          !t.children.exists( _ exists isCandidateForInsertion)
        case _ => false
      }
      
      def refineInsertionPoint(t: Tree) = t match {
        case If(_, thenp, _    ) if thenp.pos.includes(near.pos) => thenp
        case If(_, _    , elsep) if elsep.pos.includes(near.pos) => elsep
        case t => t
      }
      
      insertionPoint map refineInsertionPoint
    }
    
    val insertionPoint = findBlockInsertionPosition(selection.file, selectedExpression) getOrElse {
      return Left(RefactoringError("No insertion point found."))
    }
    
    val replaceExpression = replaceTree(selectedExpression, valRef)
    
    val findChild = filter {
       case t => t == insertionPoint
    }
    
    val insertNewVal = transform {
      case t @ BlockExtractor(stats) => mkBlock(newVal :: stats) replaces t
      case t => mkBlock(newVal :: t :: Nil)
    }
    
    val extractLocal = ↓(matchingChildren(findChild &> replaceExpression &> insertNewVal))
    
    val r = extractLocal apply abstractFileToTree(selection.file) toList
    
    Right(r)
  }
}
