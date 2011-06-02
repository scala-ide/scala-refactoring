/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

/**
 * A class that handles indentation and is passed between
 * the pretty printer and the source generator.
 * 
 * defaultIncrement specifies how much the indentation should
 * be incremented for newly generated code (pretty printer).
 */
trait Indentations {
  
  this: common.Tracing =>

  class Indentation(val defaultIncrement: String, val current: String) {
  
    def incrementDefault = new Indentation(defaultIncrement, current + defaultIncrement)
    
    def setTo(i: String) = new Indentation(defaultIncrement, i)
    
    def needsToBeFixed(oldIndentation: String, surroundingLayout: Layout*) = {
      oldIndentation != current && surroundingLayout.exists(_.contains("\n"))
    }
    
    def fixIndentation(code: String, oldIndentation: String) = {
      trace("code is %s", code)
      trace("desired indentation is %s", current)
      trace("current indentation is %s", oldIndentation)
      Layout(code.replace("\n"+ oldIndentation, "\n"+ current))
    }
  }
}