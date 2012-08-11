/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

trait Tracing {
  
  var level = 0
  val marker = "│"
  val indent = "   "

  implicit def anythingToTrace[T](t: T) = new {
    def \\ (trace: T => Unit) = {
      trace(t)
      t
    }
  }
  
  def context[T](name: String)(body: => T): T = {
 
    val spacer = "─" * (indent.length - 1) 
    
    print ((indent * level) +"╰"+ spacer +"┬────────" )
    level += 1
    trace("→ "+ name)
    
    body \\ { _ =>
      level -= 1
      print ((indent * level) + "╭"+ spacer +"┴────────" )
    }
  }

  def trace(msg: => String, arg1: => Any, args: Any*) {
        
    val as: Array[AnyRef] = arg1 +: args map {
      case s: String => "«"+ s.replaceAll("\n", "\\\\n") +"»"
      case a: AnyRef => a
    } toArray
    
    trace(msg.format(as: _*))
  }
  
  def trace(msg: => String) {
    val border = (indent * level) + marker
    print(border + msg.replaceAll("\n", "\n"+ border))
  }
  
  private[common] def print(s: => String): Unit
}

trait ConsoleTracing extends Tracing {
  override def print(s: => String) = println(s)
}

trait SilentTracing extends Tracing {
  override def print(s: => String) = ()
  
  @inline
  override def trace(msg: => String, arg1: => Any, args: Any*) = ()

  @inline
  override def trace(msg: => String) = ()

  @inline
  override def context[T](name: String)(body: => T): T = body
}
