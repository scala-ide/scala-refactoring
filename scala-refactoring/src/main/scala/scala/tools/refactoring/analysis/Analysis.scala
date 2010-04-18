/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.analysis

import scala.tools.refactoring.common.Selections

trait Analysis extends Indexes with TreeAnalysis {
  self: Selections =>
}
