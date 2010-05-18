/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring

import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.analysis.Analysis
import scala.tools.refactoring.analysis.FullIndexes
import scala.tools.refactoring.regeneration.{FragmentRepository, Regeneration}
import scala.tools.refactoring.transformation.Transformation
import scala.tools.refactoring.common.{Selections, Tracing, LayoutPreferences, SilentTracing}
import scala.tools.refactoring.common.Change

abstract class Refactoring extends Analysis with Transformation with Regeneration with Selections with SilentTracing with LayoutPreferences with FullIndexes with sourcegen.SourceGen with sourcegen.AstTransformations with common.PimpedTrees {

  val global: Global
  
  def treeForFile(file: AbstractFile) = {
    global.unitOfFile get file map (_.body)
  }
  
  def refactor(changed: List[global.Tree]): List[Change] = context("main") {
    createChanges(changed) toList
  }
}
