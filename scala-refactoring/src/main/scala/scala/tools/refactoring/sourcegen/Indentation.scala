package scala.tools.refactoring
package sourcegen

/**
 * A class that handles indentation and is passed between
 * the pretty printer and the source generator.
 * 
 * defaultIncrement specifies how much the indentation should
 * be incremented for newly generated code (pretty printer).
 * */
class Indentation(defaultIncrement: String) {
  
  private var current: String = ""
    
  def text = current
  
  /**
   * Increments the indentation and executes the thunk;
   * resets the indentation afterwards.
   * */
  def default[T](f: => T): T = {
    val old = current
    current += defaultIncrement
    val res = f
    current = old
    res
  }
  
  def setTo[T](i: String)(f: => T): T = {
    val old = current
    current = i
    val res = f
    current = old
    res
  }
}