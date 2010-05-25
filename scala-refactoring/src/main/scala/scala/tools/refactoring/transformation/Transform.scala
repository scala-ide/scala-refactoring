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
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  implicit def blockToTreeList(b: Block) = b.stats ::: b.expr :: Nil
  
  implicit def abstractFileToTree(file: AbstractFile): global.Tree = global.unitOfFile(file).body
    
  def replace[T](from: List[T], what: List[T], replacement: List[T]): List[T] = (from, what) match {
    case (Nil, _) => Nil
    case (xs, Nil) => xs
    case (x :: xs, y :: ys) if x == y => replacement ::: replace(xs, ys, Nil)
    case (x :: xs, _) => x :: replace(xs, what, replacement)
  }
}
