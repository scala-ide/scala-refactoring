package scala.tools.refactoring
package sourcegen

class Indentation(defaultIncrement: String) {
  
  private var current: String = ""
    
  def text = current
  
  def default[T](f: => T): T = {
    val old = current
    current = current + defaultIncrement
    val res = f
    current = old
    res
  }
}