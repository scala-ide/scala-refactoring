package scala.tools.refactor

import printer._

trait Tracing {
  
  var level = 0
  val marker = "│"
  val indent = "   "
    
  def using[T](arg: => T)(body: T => Unit): T = {
    val r = arg
    body(arg)
    r
  }
  
  def returns[T](arg: => T)(body: => Unit): T = {
    val r = arg
    body
    r
  }
  
  def context[T](name: String)(body: => T): T = {
 
    val spacer = "─" * (indent.length - 1) 
    
    print ((indent * level) +"╰"+ spacer +"┬────────" )
    level += 1
    trace("→ "+ name)
    
    returns(body) {
      level -= 1
      print ((indent * level) + "╭"+ spacer +"┴────────" )
    }
  }
  
  def trace(msg: String, args: Any*) {
    
    val as: Array[AnyRef] = args map {
      case s: String => "«"+ s.replaceAll("\n", "\\\\n") +"»"
      case f: Fragment => "❮"+ f.toString +"❯"
      case a: AnyRef => a
      
    } toArray
    
    print ((indent * level) + marker + msg.format(as: _* ))
  }
  
  def print(s: String) = println(s)
}

trait SilentTracing extends Tracing {
  override def print(s: String) = ()
}
