/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen
import scala.language.postfixOps

trait SourceUtils {
  /**
   * Counts brackets, skipping comments, back-tick identifiers, string and character constants.
   */
  def countRelevantBrackets(source: String, open: Char = '(', close: Char = ')'): (Int, Int) = {
    def charAtIs(i: Int, c: Char) = {
      if (i < source.length) source(i) == c
      else false
    }

    /*
     * Note for purists:
     *   The algorithm below could easily be made purely functional by packing it into a tail recursive function
     *   with an appropriate accumulator. In fact, I tried exactly this in the beginning, but found this
     *   version more straight forward and readable.
     */

    var i = 0
    var inMultilineComment = 0
    var inCharConstant = false
    var inSingleLineComment = false
    var inNormalStringConstant = false
    var inMultilineStringConstant = false
    var inBackTickIdentifier = false
    var openingBraces = 0
    var closingBraces = 0

    def inRange(j: Int) = j < source.length
    def atEnd = !inRange(i)
    def currentCharIs(c: Char) = charAtIs(i, c)
    def current2CharsAre(c0: Char, c1: Char) = inRange(i+1) && charAtIs(i+1, c1) && charAtIs(i, c0)
    def current3CharsAre(c0: Char, c1: Char, c2: Char) = inRange(i+2) && charAtIs(i+2, c2) && charAtIs(i+1, c1) && charAtIs(i, c0)
    def inComment = inSingleLineComment || inMultilineComment > 0
    def inStringConstant = inNormalStringConstant || inMultilineStringConstant
    def inRelevantSection = !inComment && !inStringConstant && !inCharConstant && !inBackTickIdentifier
    def currentCharsAreTrippleQuote = current3CharsAre('"', '"', '"')
    def currentCharsAreSingleLineCommentStart = current2CharsAre('/', '/')
    def currentCharsAreMultilineCommentCommentStart = current2CharsAre('/', '*')
    def currentCharsAreMultilineCommentCommentEnd = current2CharsAre('*', '/')
    def currentCharIsRegularQuote = currentCharIs('"')
    def currentCharsAreSingleQuotedSingleQuote = current3CharsAre(''', ''', ''')
    def currentCharsAreEscapedSingleQuote = current2CharsAre('\\', ''')
    def currentCharIsSingleQuote = currentCharIs(''')
    def currentCharsAreEscapedRegularQuote = current2CharsAre('\\', '"')
    def currentCharIsNewline = currentCharIs('\n')
    def currentCharIsBackTick = currentCharIs('`')

    while(!atEnd) {
      if (inRelevantSection) {
        if (currentCharIs(open)) {
          openingBraces += 1
        }  else if (currentCharIs(close)) {
          closingBraces +=1
        } else if (currentCharsAreTrippleQuote) {
          i += 2
          inMultilineStringConstant = true
        } else if (currentCharIsRegularQuote) {
          inNormalStringConstant = true
        } else if (currentCharsAreSingleLineCommentStart) {
          i += 1
          inSingleLineComment = true
        } else if (currentCharsAreMultilineCommentCommentStart) {
          i += 1
          inMultilineComment = 1
        } else if (currentCharsAreSingleQuotedSingleQuote) {
          i += 2
        } else if (currentCharIsSingleQuote) {
          inCharConstant = true
        } else if (currentCharIsBackTick) {
          inBackTickIdentifier = true
        }
      } else if (inSingleLineComment) {
        if (currentCharIsNewline) {
          inSingleLineComment = false
        }
      } else if (inMultilineComment > 0) {
        if (currentCharsAreMultilineCommentCommentStart) {
          i += 1
          inMultilineComment += 1
        } else if (currentCharsAreMultilineCommentCommentEnd) {
          i += 1
          inMultilineComment -= 1
        }
      } else if (inNormalStringConstant) {
        if (currentCharsAreEscapedRegularQuote) {
          i += 1
        } else if (currentCharIsRegularQuote) {
          inNormalStringConstant = false
        }
      } else if (inMultilineStringConstant) {
        if (currentCharsAreTrippleQuote) {
          i += 2
          inMultilineStringConstant = false
        }
      } else if (inCharConstant) {
        if (currentCharsAreEscapedSingleQuote) {
          i += 1
        } else if (currentCharIsSingleQuote) {
          inCharConstant = false
        }
      } else if (inBackTickIdentifier) {
        if (currentCharIsBackTick) {
          inBackTickIdentifier = false
        }
      }

      i += 1
    }

    (openingBraces, closingBraces)
  }

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

object SourceUtils extends SourceUtils
