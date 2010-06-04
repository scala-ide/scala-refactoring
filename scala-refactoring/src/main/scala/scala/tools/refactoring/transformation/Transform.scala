/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.transformation

import scala.tools.nsc.io.AbstractFile
import scala.collection.mutable.HashSet
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

trait Transform {
  this: TreeTransformations =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def replaceTree(from: Tree, to: Tree) = ↓(matchingChildren(transform {
    case t if t == from => to
  }))
    
  implicit def abstractFileToTree(file: AbstractFile): global.Tree = global.unitOfFile(file).body
  
  /**
   * Replace the first sequence of elements with another sequence.
   * */
  implicit def additionalListMethods[T](l: List[T]) = new {
    def replaceSequence(what: List[T], replacement: List[T]): List[T] = {
      def inner(from: List[T], what: List[T], replacement: List[T]): List[T] = (from, what) match {
        case (Nil, _) => Nil
        case (xs, Nil) => xs
        case (x :: xs, y :: ys) if x == y => replacement ::: inner(xs, ys, Nil)
        case (x :: xs, _) => x :: inner(xs, what, replacement)
      }
      inner(l, what, replacement)
    }
  }
}
