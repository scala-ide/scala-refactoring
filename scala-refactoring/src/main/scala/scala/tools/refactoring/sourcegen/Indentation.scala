package scala.tools.refactoring
package sourcegen

/**
 * A class that handles indentation and is passed between
 * the pretty printer and the source generator.
 * 
 * defaultIncrement specifies how much the indentation should
 * be incremented for newly generated code (pretty printer).
 * */
class Indentation(val defaultIncrement: String, val current: String) {

  def incrementDefault = new Indentation(defaultIncrement, current + defaultIncrement)
  
  def setTo(i: String) = new Indentation(defaultIncrement, i)
}