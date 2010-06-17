/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package implementations

abstract class ExtractLocal extends MultiStageRefactoring with transformation.TreeFactory {
  
  import global._
  
  abstract class PreparationResult {
    def selectedExpression: Tree
  }
  
  abstract class RefactoringParameters {
    def name: String
  }
  
  def prepare(s: Selection) = {
    
    (if(s.pos.start == s.pos.end) {
      s.root.filter {
        case t: SymTree => t.pos.start > s.pos.start
        case t: TermTree => t.pos.start > s.pos.start
        case _ => false
      } lastOption
    } else {
      s.root.find {
        case t: SymTree => t.pos sameRange s.pos
        case t: TermTree => t.pos sameRange s.pos
        case _ => false
      }
    }) match {
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
        case _: Block => true
        case _: Template => true
        case _: Try => true
        case CaseDef(_, _, body) if body.pos.includes(near.pos) => true
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
    
    def insertCloseToReference(ts: List[Tree]): List[Tree] = ts match {
      case Nil => Nil
      case x :: xs if x.pos.includes(selectedExpression.pos) => newVal :: x :: xs
      case x :: xs => x :: insertCloseToReference(xs)
    }
    
    val insertNewVal = transform {
      case t @ BlockExtractor(stats) =>
        mkBlock(insertCloseToReference(stats)) replaces t
      case tpl: Template =>
        tpl copy (body = insertCloseToReference(tpl.body)) replaces tpl
      case t @ CaseDef(_, _, body) if !body.isInstanceOf[Block] =>
        t copy (body = mkBlock(newVal :: body :: Nil)) replaces t
      case t @ Try(block, _, _) if !block.isInstanceOf[Block] =>
        t copy (block = mkBlock(newVal :: block :: Nil)) replaces t
      case t => mkBlock(newVal :: t :: Nil)
    }
    
    val extractLocal = ↓(matchingChildren(findChild &> replaceExpression &> insertNewVal))
    
    val r = extractLocal apply abstractFileToTree(selection.file) toList
    
    Right(r)
  }
}
