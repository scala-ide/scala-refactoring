/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring

import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.io.AbstractFile
import transformation.Transformation
import common.{Selections, Tracing, SilentTracing}
import common.Change

abstract class Refactoring extends Selections with Transformation with SilentTracing with sourcegen.SourceGenerator with common.PimpedTrees {

  val global: scala.tools.nsc.interactive.Global
  
  def treeForFile(file: AbstractFile) = {
    global.unitOfFile get file map (_.body)
  }
  
  def refactor(changed: List[global.Tree]): List[Change] = context("main") {
    createChanges(changed) toList
  }
}
