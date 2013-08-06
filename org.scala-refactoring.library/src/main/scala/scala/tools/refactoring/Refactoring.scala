/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring

import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.common.SilentTracing
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.common.PimpedTrees
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.sourcegen.SourceGenerator
import scala.tools.refactoring.transformation.TreeTransformations
import scala.tools.refactoring.common.TextChange

/**
 * The Refactoring trait combines the transformation and source generation traits with
 * their dependencies. Refactoring is mixed in by all concrete refactorings and can be
 * used by users of the library.
 */
trait Refactoring extends Selections with TreeTransformations with SilentTracing with SourceGenerator with PimpedTrees {

  this: common.CompilerAccess =>

  /**
   * Creates a list of changes from a list of (potentially changed) trees.
   *
   * @param A list of trees that are to be searched for modifications.
   * @return A list of changes that can be applied to the source file.
   */
  def refactor(changed: List[global.Tree]): List[TextChange] = context("main") {
    val changes = createChanges(changed)
    changes map minimizeChange
  }

  /**
   * Creates changes by applying a transformation to the root tree of an
   * abstract file.
   */
  def transformFile(file: AbstractFile, transformation: Transformation[global.Tree, global.Tree]): List[TextChange] = {
    refactor(transformation(abstractFileToTree(file)).toList)
  }

  /**
   * Makes a generated change as small as possible by eliminating the
   * common pre- and suffix between the change and the source file.
   */
  private def minimizeChange(change: TextChange): TextChange = change match {
    case TextChange(file, from, to, changeText) =>

      def commonPrefixLength(s1: Seq[Char], s2: Seq[Char]) =
        (s1 zip s2 takeWhile Function.tupled(_==_)).length

      val original    = file.content.subSequence(from, to).toString
      val replacement = changeText

      val commonStart = commonPrefixLength(original, replacement)
      val commonEnd   = commonPrefixLength(original.substring(commonStart).reverse, replacement.substring(commonStart).reverse)

      val minimizedChangeText = changeText.subSequence(commonStart, changeText.length - commonEnd).toString
      TextChange(file, from + commonStart, to - commonEnd, minimizedChangeText)
  }
}
