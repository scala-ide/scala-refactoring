/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

trait Tracing {

  implicit class TraceAndReturn[T](t: T) {
    def \\ (trace: T => Unit) = {
      trace(t)
      t
    }
  }

  def context[T](name: String)(body: => T): T

  def trace(msg: => String, arg1: => Any, args: Any*)

  def trace(msg: => String)
}

trait ConsoleTracing extends Tracing {

  var level = 0
  val marker = "│"
  val indent = "   "

  override def context[T](name: String)(body: => T): T = {

    val spacer = "─" * (indent.length - 1)

    println((indent * level) +"╰"+ spacer +"┬────────" )
    level += 1
    trace("→ "+ name)

    body \\ { _ =>
      level -= 1
      println((indent * level) + "╭"+ spacer +"┴────────" )
    }
  }

  override def trace(msg: => String, arg1: => Any, args: Any*) {

    val as: Array[AnyRef] = arg1 +: args.toArray map {
      case s: String => "«"+ s.replaceAll("\n", "\\\\n") +"»"
      case a: AnyRef => a
    }

    trace(msg.format(as: _*))
  }

  override def trace(msg: => String) {
    val border = (indent * level) + marker
    println(border + msg.replaceAll("\n", "\n"+ border))
  }
}

trait SilentTracing extends Tracing {

  @inline
  def trace(msg: => String, arg1: => Any, args: Any*) = ()

  @inline
  def trace(msg: => String) = ()

  @inline
  def context[T](name: String)(body: => T): T = body
}
