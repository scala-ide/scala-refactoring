/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring

import common.{Selections, SilentTracing, Change, PimpedTrees}
import sourcegen.SourceGenerator
import transformation.TreeTransformations

/**
 * The Refactoring trait combines the transformation and source generation traits with
 * their dependencies. Refactoring is mixed in by all concrete refactorings and can be
 * used by users of the library.
 */
trait Refactoring extends Selections with TreeTransformations with SilentTracing with SourceGenerator with PimpedTrees {
  
  this: common.CompilerAccess =>
    
  def refactor(changed: List[global.Tree]): List[Change] = context("main") {
    createChanges(changed) toList
  }
}
