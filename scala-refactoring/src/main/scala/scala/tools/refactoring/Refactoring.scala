/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring

import transformation.TreeTransformations
import common.{Selections, Tracing, SilentTracing, Change, PimpedTrees}
import sourcegen.SourceGenerator

abstract class Refactoring extends Selections with TreeTransformations with SilentTracing with SourceGenerator with PimpedTrees {
  
  def refactor(changed: List[global.Tree]): List[Change] = context("main") {
    createChanges(changed) toList
  }
}
