/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package transformation

trait Transformation extends Transform with TreeFactory with TreeTransformations {
  this: common.PimpedTrees =>
}
