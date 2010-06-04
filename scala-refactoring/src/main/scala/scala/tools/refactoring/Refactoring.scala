/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring

import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.AbstractFile
import analysis.Analysis
import analysis.FullIndexes
import transformation.Transformation
import common.{Selections, Tracing, SilentTracing}
import common.Change

abstract class Refactoring extends Analysis with Selections with Transformation with SilentTracing with FullIndexes with sourcegen.SourceGen with common.PimpedTrees {

  val global: Global
  
  def treeForFile(file: AbstractFile) = {
    global.unitOfFile get file map (_.body)
  }
  
  def refactor(changed: List[global.Tree]): List[Change] = context("main") {
    createChanges(changed) toList
  }
}
