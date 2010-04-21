/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.common

trait ConsoleTracing extends Tracing {
  override def print(s: String) = println(s)
}