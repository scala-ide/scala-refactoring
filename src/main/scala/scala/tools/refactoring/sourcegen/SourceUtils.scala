/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen
import scala.language.postfixOps

object SourceUtils {

  def stripFromCode(source: String, c: Char) = {

    val (rest, comments) = splitComment(source)

    (rest zip comments) map {
      case (' ', _1) => ""+ _1
      case (`c`, _ ) => ""
      case (_1, ' ') => ""+ _1
      case ('\n', '\n') => "\n"
      case _ => assert(false)
    } mkString
  }

  def stripComment(source: String): String = splitComment(source.toCharArray)._1

  def stripComment(source: Array[Char]): String = splitComment(source)._1

  def splitComment(source: String): (String, String) = splitComment(source.toCharArray)

  private def splitComment(source: Array[Char]): (String, String) = {
    if (source.isEmpty) {
      ("", "")
    } else if (source.length == 1) {
      (source(0).toString, " ")
    } else {
      var inMultilineComment = 0
      var inSingleLineComment = false
      var nextToComment = false
      def inComment = inSingleLineComment || inMultilineComment > 0
      var inNormalStringConstant = false
      var inMultilineStringConstant = false
      def inStringConstant = inNormalStringConstant || inMultilineStringConstant

      val text = new StringBuilder
      val comment = new StringBuilder

      def add(c: Char, t: Char) = {
        comment append c
        text append t
      }

      var i = 0

      def nextCharIs(c: Char) = {
        (i + 1 < source.length) && source(i + 1) == c
      }

      def nextCharsAre(c1: Char, c2: Char) = {
        (i + 2 < source.length) && source(i + 1) == c1 && source(i + 2) == c2
      }

      val src = source ++ " "
      while (i < source.length) {

        val _1 = src(i)

        if (nextToComment) {
          nextToComment = false
          add(_1, ' ')
        } else if (!inStringConstant && _1 == '/' && nextCharIs('/') && !inSingleLineComment && inMultilineComment == 0) {
          inSingleLineComment = true
          nextToComment = true
          add('/', ' ')
        } else if (_1 == '\r') {
          inSingleLineComment = false
          add('\r', '\r')
        } else if (_1 == '\n') {
          inSingleLineComment = false
          add('\n', '\n')
        } else if (!inStringConstant && _1 == '/' && nextCharIs('*') && !inSingleLineComment) {
          inMultilineComment += 1
          nextToComment = true
          add('/', ' ')
        } else if (_1 == '*' && nextCharIs('/') && !inSingleLineComment && inMultilineComment > 0) {
          inMultilineComment -= 1
          nextToComment = true
          add('*', ' ')
        } else if(_1 == '"' && !inComment) {
          if (inNormalStringConstant) {
            inNormalStringConstant = false
          } else if (inMultilineStringConstant) {
            if (nextCharsAre('"', '"')) {
              inMultilineStringConstant = false
            }
          } else {
            if (nextCharsAre('"', '"')) {
              inMultilineStringConstant = true
            } else {
              inNormalStringConstant = true
            }
          }

          add(' ', '"')
        } else if (inComment) {
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
