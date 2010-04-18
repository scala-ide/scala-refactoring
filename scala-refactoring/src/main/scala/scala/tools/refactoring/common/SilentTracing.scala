/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.common

trait SilentTracing extends Tracing {
  override def print(s: String) = ()
}