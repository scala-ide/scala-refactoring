/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory

abstract class ExtractLocal extends MultiStageRefactoring with TreeFactory with common.TreeExtractors with common.InteractiveScalaCompiler {

  import global._

  type PreparationResult = Tree

  type RefactoringParameters = String

  def prepare(s: Selection) = {

    (if(s.pos.start == s.pos.end) {
      s.root.filter {
        case t @ (_: SymTree | _: TermTree) if t.pos.isRange => t.pos.start > s.pos.start
        case _ => false
      }.headOption
    } else {
      s.root.find {
        case t @ (_: SymTree | _: TermTree) =>
          (t.pos sameRange s.pos) && !hasUnitType(t)
        case _ => false
      }
    }) toRight (PreparationError("no term selected"))
  }

  def perform(selection: Selection, selectedExpression: PreparationResult, name: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    trace("Selected: %s", selectedExpression)

    val newVal = {
      selectedExpression match {
        /*
         * In case we extract a method/function call without its arguments
         * */
        case t: Ident if t.symbol.isMethod =>
          mkValDef(name, Apply(selectedExpression, Ident("_") :: Nil))
        case _ =>
          mkValDef(name, selectedExpression)
      }
    }
    val valRef = Ident(name)

    def findBlockInsertionPosition(root: Tree, near: Tree) = {

      def isCandidateForInsertion(t: Tree) = {
        t != selectedExpression && t.pos.includes(near.pos) && !t.pos.isTransparent && PartialFunction.cond(t) {
          case If(_, thenp, _    ) if thenp.pos.includes(near.pos) => true
          case If(_, _    , elsep) if elsep.pos.includes(near.pos) => true
          case _: Block => true
          case _: Template => true
          case _: Try => true
          case Function(vals, _) => vals.forall(_.pos.isRange) /* for (_:Int)+1 */
          case _: DefDef => true
          case CaseDef(_, _, body) if body.pos.includes(near.pos) => true
        }
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

    val insertionPoint = findBlockInsertionPosition(selection.root, selectedExpression) getOrElse {
      return Left(RefactoringError("No insertion point found."))
    }

    val findInsertionPoint = predicate((t: Tree) => t == insertionPoint)

    def insertCloseToReference(ts: List[Tree]): List[Tree] = ts match {
      case Nil => Nil
      case x :: xs if x.pos.overlaps(selectedExpression.pos) => newVal :: x :: xs
      case x :: xs if x == valRef => newVal :: valRef :: xs
      case x :: xs => x :: insertCloseToReference(xs)
    }

    val insertNewVal = transform {

      case t @ BlockExtractor(stats) =>
        mkBlock(insertCloseToReference(stats)) replaces t

      case tpl: Template =>
        tpl copy (body = insertCloseToReference(tpl.body)) replaces tpl

      case t @ CaseDef(_, _, NoBlock(body)) =>
        t copy (body = mkBlock(newVal :: body :: Nil)) replaces t

      case t @ Try(NoBlock(block), _, _) =>
        t copy (block = mkBlock(newVal :: block :: Nil)) replaces t

      case t @ DefDef(_, _, _, _, _, NoBlock(rhs)) =>
        val newRhs = shallowDuplicate(rhs) setPos NoPosition
        t copy (rhs = mkBlock(newVal :: newRhs :: Nil)) replaces t

      case t @ Function(_, NoBlock(body)) =>

        val hasOpeningCurlyBrace = {
          val src = t.pos.source.content.slice(0, t.pos.start).mkString
          src.matches("(?ms).*\\{\\s*$")
        }

        if(hasOpeningCurlyBrace) {
          t copy (body = mkBlock(newVal :: body :: Nil)) replaces t
        } else {
          // this will create a block inside the function body, e.g.
          //   (i => {
          //     ...
          //   })
          t copy (body = mkBlock(newVal :: body :: Nil))
        }
      case t => mkBlock(newVal :: t :: Nil)
    }

    val extractLocal = topdown(matchingChildren(findInsertionPoint &> replaceTree(selectedExpression, valRef) &> insertNewVal))

    Right(transformFile(selection.file, extractLocal))
  }
}
