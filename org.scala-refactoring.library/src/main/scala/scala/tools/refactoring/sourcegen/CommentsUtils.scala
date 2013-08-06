/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

object CommentsUtils {

  def stripComment(source: String): String = splitComment(source.toCharArray)._1

  def stripComment(source: Array[Char]): String = splitComment(source)._1

  def splitComment(source: String): (String, String) = splitComment(source.toCharArray)

  private def splitComment(source: Array[Char]): (String, String) = {
    if(source.isEmpty) {
      ("", "")
    } else if(source.length == 1) {
      (source(0).toString, " ")
    } else {
      var nestingLevel = 0
      var lineComment = false
      var nextToComment = false

      val text = new StringBuilder
      val comment = new StringBuilder

      def add(c: Char, t: Char) = {
        comment append c
        text append t
      }

      var i = 0
      val src = source ++ " "
      while(i < source.length) {

        val _1 = src(i)

        if(nextToComment) {
          nextToComment = false
          add(_1, ' ')
        } else if (_1 == '/' && src(i+1) == '/' && !lineComment && nestingLevel == 0 ) {
          lineComment = true
          nextToComment = true
          add('/', ' ')
        } else if (_1 == '\r') {
          lineComment = false
          add('\r', '\r')
        } else if (_1 == '\n') {
          lineComment = false
          add('\n', '\n')
        } else if (_1 == '/' && src(i+1) == '*' && !lineComment) {
          nestingLevel += 1
          nextToComment = true
          add('/', ' ')
        } else if (_1 == '*' && src(i+1) == '/' && !lineComment && nestingLevel > 0) {
          nestingLevel -= 1
          nextToComment = true
          add('*', ' ')
        } else if (lineComment || nestingLevel > 0) {
          add(_1, ' ')
        } else {
          add(' ', _1)
        }

        i += 1
      }

      /*
       * This is the previous implementation, which is easier to understand bad
       * performs worse due to a lot of boxing and Array.unapply calls.
       * */

      /*(source ++ " ") sliding(2) foreach {
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
      }*/

      (text.mkString, comment.mkString)
    }
  }
}
