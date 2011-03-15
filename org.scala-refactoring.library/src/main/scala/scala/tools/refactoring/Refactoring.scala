/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring

import common.{Selections, SilentTracing, Change, PimpedTrees}
import sourcegen.SourceGenerator
import transformation.TreeTransformations
import tools.nsc.io.AbstractFile

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
  def refactor(changed: List[global.Tree]): List[Change] = context("main") {
    createChanges(changed)
  }
  
  /**
   * Creates changes by applying a transformation to the root tree of an
   * abstract file.
   */
  def transformFile(file: AbstractFile, transformation: Transformation[global.Tree, global.Tree]): List[Change] = {
    refactor(transformation(abstractFileToTree(file)).toList)
  }
}
