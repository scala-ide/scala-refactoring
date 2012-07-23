/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

object CommentsUtils {
  
  def stripComment(source: String): String = splitComment(source)._1
  
  def splitComment(source: Seq[Char]): (String, String) = source match {
    case Nil => ("", "")
    case a :: Nil => (a.toString, " ")
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
      
      (s ++ " ") reduceLeft({
        case (_1, _2) if nextToComment =>
          nextToComment = false
          add(_1, ' ')
          _2
        
        case ('/', '/') if !lineComment && nestingLevel == 0 => 
          lineComment = true
          nextToComment = true
          add('/', ' ')
          '/'
        
        case ('\r', _2) =>
          lineComment = false
          add('\r', '\r')
          _2
          
        case ('\n', _2) =>
          lineComment = false
          add('\n', '\n')
          _2
          
        case ('/', '*') if !lineComment =>
          nestingLevel += 1
          nextToComment = true
          add('/', ' ')
          '*'
          
        case ('*', '/') if !lineComment && nestingLevel > 0 =>
          nestingLevel -= 1
          nextToComment = true
          add('*', ' ')
          '/'
          
        case (_1, _2) if lineComment || nestingLevel > 0 => 
          add(_1, ' ')
          _2
          
        case (_1, _2) =>
          add(' ', _1)
          _2
      }: (Char, Char) => Char)
            
      (text mkString, comment mkString)
  }
}
