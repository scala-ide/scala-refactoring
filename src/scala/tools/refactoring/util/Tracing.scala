package scala.tools.refactoring.util

import scala.tools.refactoring.regeneration._

object StopWatch {
  private lazy val start = System.currentTimeMillis
  def elapsed = System.currentTimeMillis - start
}

trait Tracing {
  
  var level = 0
  val marker = "│"
  val indent = "   "
  
  // TODO move to some other utility trait/object 
  def returns[T](arg: => T) = new {
    def apply(body: => Unit): T = {
      val r = arg
      body
      r
    }
    def apply(body: T => Unit): T = {
      val r = arg
      body(r)
      r
    }
  }
  
  // TODO could we print the whole sequence with http://www.websequencediagrams.com ?
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
      case f: Fragments#Fragment => "❮"+ f.toString +"❯"
      case a: AnyRef => a
      
    } toArray
    
    trace(msg.format(as: _*))
  }
  
  def trace(msg: String) {
    print ((indent * level) + marker + msg)
  }
  
  def print(s: String) = println(s)
}

trait SilentTracing extends Tracing {
  override def print(s: String) = ()
}
