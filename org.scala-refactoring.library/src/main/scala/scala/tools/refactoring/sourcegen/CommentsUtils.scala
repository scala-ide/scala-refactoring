/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import scala.tools.refactoring.util.Memoized

object CommentsUtils {
  
  def stripComment(source: String): String = splitComment(source.toCharArray)._1
  
  def splitComment(source: String): (String, String) = splitComment(source.toCharArray)
    
  val splitComment: Array[Char] => (String, String) = Memoized {
    case Array() => ("", "")
    case Array(a) => (a.toString, " ")
    case s =>
      var nestingLevel = 0
      var lineComment = false
      var nextToComment = false
      
      val text = new StringBuilder
      val comment = new StringBuilder
      
      def add(c: Char, t: Char) = {
        comment append c
        text append t
      }
      
      (s ++ " ") sliding(2) foreach {
        case Array(_1, _2) if nextToComment =>
          nextToComment = false
          add(_1, ' ')
        
        case Array('/', '/') if !lineComment && nestingLevel == 0 => 
          lineComment = true
          nextToComment = true
          add('/', ' ')
        
        case Array('\r', _2) =>
          lineComment = false
          add('\r', '\r')
          
        case Array('\n', _2) =>
          lineComment = false
          add('\n', '\n')
          
        case Array('/', '*') if !lineComment =>
          nestingLevel += 1
          nextToComment = true
          add('/', ' ')
          
        case Array('*', '/') if !lineComment && nestingLevel > 0 =>
          nestingLevel -= 1
          nextToComment = true
          add('*', ' ')
          
        case Array(_1, _2) if lineComment || nestingLevel > 0 => 
          add(_1, ' ')
          
        case Array(_1, _2) =>
          add(' ', _1)
      }
            
      (text.mkString, comment.mkString)
  }
}
