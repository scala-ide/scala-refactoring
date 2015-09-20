package scala.tools.refactoring.util

import scala.annotation.tailrec
import java.util.Arrays
import scala.collection.immutable.SortedMap
import scala.language.implicitConversions
import scala.reflect.internal.util.RangePosition
import scala.collection.SeqView

/**
 * Represents source code with a movable marker.
 *
 * @see [[SourceWithMarker.Movement]]
 * @see [[SourceWithMarker.Movements]]
 */
final case class SourceWithMarker(source: IndexedSeq[Char] = IndexedSeq(), marker: Int = 0) {
  import SourceWithMarker._

  def moveMarker(movement: SimpleMovement): SourceWithMarker = {
    if (isDepleted) this
    else movement(this).map(m => copy(marker = m)).getOrElse(this)
  }

  def moveMarkerBack(movement: Movement): SourceWithMarker = {
    moveMarker(movement.backward)
  }

  def current: Char = source(marker)

  def isInRange(i: Int): Boolean = {
    i >= 0 && i < source.length
  }

  def isDepleted: Boolean = {
    !isInRange(marker)
  }

  def length = source.length

  override def toString = {
    def mkFlatString(chars: Seq[Char]): String = {
      chars.mkString("").replace("\r\n", "\\r\\n").replace("\n", "\\n")
    }

    val lrChars = 3
    val nChars = lrChars*2 + 1

    def leftDots = if (marker - lrChars > 0) "..." else ""
    def rightDots = if (marker + lrChars < source.length - 1) "..." else ""

    if (marker < 0) {
      "<>" + mkFlatString(source.take(nChars)) + rightDots
    } else if (marker >= source.length) {
      leftDots + mkFlatString(source.takeRight(nChars)) + "<>"
    } else {
      val marked = current
      val lStr = leftDots + mkFlatString(source.slice(marker - lrChars, marker))
      val rStr = mkFlatString(source.slice(marker + 1, marker + 1 + lrChars)) + rightDots
      s"$lStr<$marked>$rStr"
    }
  }

  override def equals(obj: Any) = obj match {
    case SourceWithMarker(otherSource, otherMarker) =>
      source.toSeq == otherSource.toSeq && marker == otherMarker
    case _ => false
  }

  override def hashCode = {
    (source.toSeq, marker).hashCode
  }

  private def emptyView = IndexedSeq.empty[Char].view(0, 0)

  private def nextChars(n: Int): Option[SeqView[Char, IndexedSeq[Char]]] = {
    if (isDepleted) {
      None
    } else if (n == 0) {
      Some(emptyView)
    } else {
      val m = marker + n
      if (n > 0 && m <= length) Some(source.view(marker + 1, m + 1))
      else if (m >= 0) Some(source.view(m, marker))
      else None
    }
  }

  private def nextChar(n: Int): Option[Char] = {
    charAt(marker + n)
  }

  private def charAt(i: Int): Option[Char] = {
    if (isInRange(i)) Some(source(i))
    else None
  }
}

object SourceWithMarker {

  def beforeStartOf(pos: RangePosition): SourceWithMarker = {
    SourceWithMarker(pos.source.content, pos.start - 1)
  }

  def afterEndOf(pos: RangePosition): SourceWithMarker = {
    SourceWithMarker(pos.source.content, pos.end + 1)
  }

  /**
   * A [[Movement]] that can be applied in one direction.
   *
   * The difference between a [[SimpleMovement]] and a [[Movement]] is that [[SimpleMovement]]
   * is missing the `backward` method. Apart from that, the documentation for [[Movement]] applies.
   * Note that ''movements'' can be combined with ''simple movements'', so there is no need to
   * implement a full movement unless you need it.
   */
  trait SimpleMovement { self =>
    def apply(sourceWithMarker: SourceWithMarker): Option[Int]

    final def ~(other: SimpleMovement): SimpleMovement = SimpleMovementOps.sequence(this, other) _
    final def |(other: SimpleMovement): SimpleMovement = SimpleMovementOps.or(this, other) _
    def zeroOrMore: SimpleMovement = SimpleMovementOps.repeat(this) _
    def atLeastOnce: SimpleMovement = this ~ this.zeroOrMore
    def nTimes(n: Int): SimpleMovement = SimpleMovementOps.nTimes(this, n) _
    def butNot(other: SimpleMovement): SimpleMovement = SimpleMovementOps.butNot(this, other) _
    def optional: SimpleMovement = SimpleMovementOps.optional(this) _
    def atMostNtimes(n: Int): SimpleMovement = optional.nTimes(n)
  }

  object SimpleMovement {
    implicit class WrapSimpleMovementImpl(impl: SourceWithMarker => Option[Int]) extends SimpleMovement {
      override def apply(sourceWithMarker: SourceWithMarker) = impl(sourceWithMarker)
    }

    def apply(impl: SourceWithMarker => Option[Int]): SimpleMovement = impl

    def startingWith(c: Char)(impl: SourceWithMarker => Option[Int]) = SimpleMovement { sourceWithMarker =>
      if (sourceWithMarker.current == c) impl(sourceWithMarker)
      else None
    }

    def ifNotDepleted(impl: SourceWithMarker => Option[Int]): SimpleMovement = {
      SimpleMovement(s => MovementHelpers.doIfNotDepleted(s)(impl(s)))
    }
  }

  private object SimpleMovementOps {
    def sequence[MovementT <: SimpleMovement](mvnt1: MovementT, mvnt2: MovementT)(sourceWithMarker: SourceWithMarker): Option[Int] = {
      mvnt1(sourceWithMarker).map(newMarker => sourceWithMarker.copy(marker = newMarker)).flatMap(mvnt2.apply)
    }

    def or[MovementT <: SimpleMovement](mvnt1: MovementT, mvnt2: MovementT)(sourceWithMarker: SourceWithMarker): Option[Int] = {
      mvnt1(sourceWithMarker).orElse(mvnt2(sourceWithMarker))
    }

    def repeat[MovementT <: SimpleMovement](mvnt: MovementT)(sourceWithMarker: SourceWithMarker): Option[Int] = {
      @tailrec
      def go(sourceWithMarker: SourceWithMarker): Option[Int] = {
        mvnt(sourceWithMarker) match {
          case Some(marker) =>
            if (marker != sourceWithMarker.marker) go(sourceWithMarker.copy(marker = marker))
            else Some(marker)
          case None => Some(sourceWithMarker.marker)
        }
      }

      go(sourceWithMarker)
    }

    @tailrec
    def nTimes[MovementT <: SimpleMovement](mvnt: MovementT, n: Int)(sourceWithMarker: SourceWithMarker): Option[Int] = {
      if (n == 0) {
        Some(sourceWithMarker.marker)
      } else if (n == 1) {
        mvnt(sourceWithMarker)
      } else if (n > 1) {
        mvnt(sourceWithMarker) match {
          case Some(newMarker) =>
            if (newMarker == sourceWithMarker.marker) Some(newMarker)
            else nTimes(mvnt, n-1)(sourceWithMarker.copy(marker = newMarker))
          case _ => None
        }
      } else {
        throw new IllegalArgumentException(s"$n")
      }
    }

    def butNot[MovementT <: SimpleMovement](mvnt: MovementT, notMvnt: MovementT)(sourceWithMarker: SourceWithMarker): Option[Int] = {
      notMvnt(sourceWithMarker) match {
        case None => mvnt(sourceWithMarker)
        case Some(_) => None
      }
    }

    def optional[MovementT <: SimpleMovement](mvnt: MovementT)(sourceWithMarker: SourceWithMarker): Option[Int] = {
      mvnt(sourceWithMarker).orElse(Some(sourceWithMarker.marker))
    }
  }

  /**
   * A context dependent, directional movement that can be applied to a [[SourceWithMarker]]
   *
   * ==Overview==
   * Movements can be combined similar to parser combinators and optionally applied backwards.
   * They are meant to be used to perform minor tweaks in already parsed source code that might be necessary
   * due to compiler bugs or compiler API limitations.
   *
   * ==Why not use parser combinators?==
   * <ul>
   *  <li>We want to conveniently move forward <b>and</b> backward in the source code</li>
   *  <li>The code we are looking at is already parsed; we only want to move to specific points</li>
   * </ul>
   *
   * ==Examples==
   * {{{
   * scala> import scala.tools.refactoring.util.SourceWithMarker
   * scala> import scala.tools.refactoring.util.SourceWithMarker._
   * scala> import scala.tools.refactoring.util.SourceWithMarker.Movements._
   *
   * scala> val src = SourceWithMarker("private val /*---*/ x = 4".toCharArray)
   * src: scala.tools.refactoring.util.SourceWithMarker = <p>riv...
   * scala> val movement = ("private" | "protected") ~ commentsAndSpaces ~ "val" ~ commentsAndSpaces
   * scala> val srcAtx = src.moveMarker(movement)
   * srcAtx: scala.tools.refactoring.util.SourceWithMarker = ... <x> = ...
   * scala> val moveBackToVal = ("al" ~ commentsAndSpaces ~ "x").backward
   * scala> val srcAtVal = srcAtx.moveMarker(moveBackToVal)
   * srcAtVal:  scala.tools.refactoring.util.SourceWithMarker = ...te <v>al ...
   * }}}
   *
   * @see [[Movements]]
   */
  trait Movement extends SimpleMovement { self =>
    def backward: Movement

    final def ~(other: Movement) = Movement { (sourceWithMarker, forward) =>
      if (forward) {
        SimpleMovementOps.sequence(self, other)(sourceWithMarker)
      } else {
       other.backward.apply(sourceWithMarker).map(newMarker => sourceWithMarker.copy(marker = newMarker)).flatMap(self.backward.apply)
      }
    }

    final def |(other: Movement) = Movement { (sourceWithMarker, forward) =>
      if (forward) self(sourceWithMarker).orElse(other(sourceWithMarker))
      else (other.backward(sourceWithMarker)).orElse(self.backward(sourceWithMarker))
    }

    final override def zeroOrMore = Movement { (sourceWithMarker, forward) =>
      val mvnt = if (forward) self else self.backward
      SimpleMovementOps.repeat(mvnt)(sourceWithMarker)
    }

    final override def atLeastOnce: Movement = this ~ this.zeroOrMore

    final override def nTimes(n: Int) = Movement { (sourceWithMarker, forward) =>
      val mvnt = if  (forward) self else self.backward
      SimpleMovementOps.nTimes(mvnt, n)(sourceWithMarker)
    }

    final def butNot(mvnt: Movement) = Movement { (sourceWithMarker, forward) =>
      val (actualSelf, actualMvnt) = {
        if (forward) (self, mvnt)
        else (self.backward, mvnt.backward)
      }

      SimpleMovementOps.butNot(actualSelf, actualMvnt)(sourceWithMarker)
    }

    final override def optional = Movement { (sourceWithMarker, forward) =>
      SimpleMovementOps.optional(if (forward) self else self.backward)(sourceWithMarker)
    }

    final override def atMostNtimes(n: Int) = optional.nTimes(n)
  }

  object Movement {
    def apply(impl: (SourceWithMarker, Boolean) => Option[Int]): Movement = {
      class MovementImpl(forward: Boolean) extends Movement {
        override def apply(sourceWithMarker: SourceWithMarker) = impl(sourceWithMarker, forward)
        override def backward = new MovementImpl(!forward)
      }

      return new MovementImpl(true)
    }

    def ifNotDepleted(impl: (SourceWithMarker, Boolean) => Option[Int]): Movement = {
      Movement((s, f) => MovementHelpers.doIfNotDepleted(s)(impl(s, f)))
    }
  }

  /**
   * Various movements related to Scala code
   *
   * Take a look at the
   * [[http://www.scala-lang.org/files/archive/spec/2.11/01-lexical-syntax.html  Scala Language Specification]]
   * if you wonder about terms like ''plainid'', ''idrest'' or ''varid''.
   */
  object Movements {
    import MovementHelpers._

    class SingleCharMovement(private val acceptChar: Int => Boolean, private val forward: Boolean = true) extends Movement {
      final override def apply(sourceWithMarker: SourceWithMarker): Option[Int] = {
        if (sourceWithMarker.isDepleted || !acceptChar(sourceWithMarker.current)) None
        else Some(nextMarker(sourceWithMarker.marker, forward))
      }

      final def |(other: SingleCharMovement): SingleCharMovement = {
        new SingleCharMovement(c => acceptChar(c) || other.acceptChar(c), forward)
      }

      final override def backward: SingleCharMovement = new SingleCharMovement(acceptChar, !forward)

      final def butNot(mvnt: SingleCharMovement): SingleCharMovement = {
        new SingleCharMovement(c => acceptChar(c) && !mvnt.acceptChar(c))
      }
    }

    private implicit class CharacterOps(val underlying: Int) extends AnyVal {
      def getType = Character.getType(underlying)
      def isUpper = Character.isUpperCase(underlying)
      def isLower = Character.isLowerCase(underlying)
      def isTitleCase = Character.isTitleCase(underlying)
      def isDigit = Character.isDigit(underlying)
      def isControl = Character.isISOControl(underlying)
    }

    val any = new SingleCharMovement(_ => true)

    val none = new SingleCharMovement(_ => false)

    def character(c: Char) = new SingleCharMovement(_ == c)

    def string(str: String) = Movement.ifNotDepleted { (sourceWithMarker, forward) =>
      def strAt(i: Int) = {
        if (forward) str.charAt(i)
        else str.charAt(str.length - 1 - i)
      }

      @tailrec
      def go(m: Int, i: Int = 0): Option[Int] = {
        if (i >= str.length) {
          Some(m)
        } else if (wouldBeDepleted(m, sourceWithMarker)) {
          None
        } else {
          if (strAt(i) == sourceWithMarker.source(m)) go(nextMarker(m, forward), i + 1)
          else None
        }
      }

      go(sourceWithMarker.marker)
    }

    val comment = Movement.ifNotDepleted { (sourceWithMarker, forward) =>
      @tailrec
      def go(m: Int, inSingleLineComment: Boolean = false, inMultilineComment: Int = 0, slashSeen: Boolean = false, starSeen: Boolean = false): Option[Int] = {
        if (wouldBeDepleted(m, sourceWithMarker)) {
          if (inSingleLineComment && forward) Some(m) else None
        } else {
          val c = sourceWithMarker.source(m)
          val nm = nextMarker(m, forward)

          if (inSingleLineComment) {
            if (c == '/') if(slashSeen && !forward) Some(nm) else go(nm, inSingleLineComment = true, slashSeen = true)
            else if (c == '\n') if(forward) Some(m) else None
            else go(nm, inSingleLineComment = true)
          } else if (inMultilineComment > 0) {
            if (c == '/') {
              if (starSeen) {
                if (inMultilineComment == 1) Some(nm)
                else go(nm, inMultilineComment = inMultilineComment - 1)
              } else {
                go(nm, inMultilineComment = inMultilineComment, slashSeen = true)
              }
            } else if (c == '*') {
              if (slashSeen) go(nm, inMultilineComment = inMultilineComment + 1)
              else go(nm, inMultilineComment = inMultilineComment, starSeen = true)
            } else {
              go(nm, inMultilineComment = inMultilineComment)
            }
          } else {
            if (c == '/') {
              if (slashSeen && forward) go(nm, inSingleLineComment = true)
              else go(nm, slashSeen = true)
            } else if(c == '*' && slashSeen) {
              go(nm, inMultilineComment = 1)
            } else {
              None
            }
          }
        }
      }

      if(!forward) go(sourceWithMarker.marker, inSingleLineComment = true).orElse(go(sourceWithMarker.marker, inSingleLineComment = false))
      else go(sourceWithMarker.marker)
    }

    def inBrackets(open: Char, close: Char) = Movement.ifNotDepleted { (sourceWithMarker, forward) =>
      val (br1, br2) = if (forward) (open, close) else (close, open)
      if (sourceWithMarker.isDepleted || sourceWithMarker.current != br1) {
        None
      } else {
        val eventuallyReversedCommentsAndSpaces = {
          if (forward) commentsAndSpaces
          else commentsAndSpaces.backward
        }

        @tailrec
        def go(m: Int): Option[Int] = {
          val mm = sourceWithMarker.copy(marker = m).moveMarker(eventuallyReversedCommentsAndSpaces).marker
          if (wouldBeDepleted(mm, sourceWithMarker)) {
            None
          } else {
            val nm = if (mm == m) nextMarker(mm, forward) else mm
            if(sourceWithMarker.source(nm) == br2) Some(nextMarker(nm, forward))
            else go(nm)
          }
        }

        go(nextMarker(sourceWithMarker.marker, forward))
      }
    }

    /**
     * Advances the marker until the given movement can be applied, optionally skipping parts of the input.
     *
     * Note that the movement specified by ''skipping'' is applied after ''mvnt''. This matters if
     * there is input that might be matched by both movements. Here is an example:
     * {{{
     * val src = SimpleMovement("0123456789".toCharArray)
     * src.moveMarker(until('5', skipping = digit))
     * }}}
     * will move the marker to ''5'', because ''5'' is matched before ''digit''. However doing
     * {{{
     * src.moveMarker(until('5', skipping = digit.zeroOrMore))
     * }}}
     * will leave the marker at ''0'', since ''digit.zeroOrMore'' will consume the entire string,
     * after matching ''5'' against ''0'' has failed.
     */
    def until(mvnt: SimpleMovement, skipping: SimpleMovement = none) = SimpleMovement { sourceWithMarker =>
      @tailrec
      def go(sourceWithMarker: SourceWithMarker = sourceWithMarker): Option[Int] = {
        mvnt(sourceWithMarker) match {
          case Some(_) => Some(sourceWithMarker.marker)
          case _ =>
            if (sourceWithMarker.isDepleted) {
              None
            } else {
              val newMarker = skipping(sourceWithMarker).getOrElse(sourceWithMarker.marker + 1)
              go(sourceWithMarker.copy(marker = newMarker))
            }
        }
      }

      go()
    }

    def charOfClass(inClass: Int => Boolean) = new SingleCharMovement(inClass)

    val space = charOfClass { c =>
      c == '\u0020' || c == '\u0009' || c == '\u000D' || c == '\u000A'
    }

    val letter = charOfClass { c =>
      c.isLower || c.isUpper || c.isTitleCase || c == '\u0024' || c == '$' || c == '\u005F' || c == '_' || {
        val ct = c.getType
        ct == Character.OTHER_LETTER || ct == Character.LETTER_NUMBER
      }
    }

    val digit = charOfClass(c => c.isDigit)

    val bracket = charOfClass { c =>
      c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}'
    }

    val delimiter = charOfClass { c =>
      c == '`' || c == ''' || c == '"' || c == '.' || c == ';' || c == ','
    }

    val opChar = charOfClass { c =>
      (c >= '\u0020' && c <= '\u007F') || {
        val ct = c.getType
        ct == Character.MATH_SYMBOL || ct == Character.OTHER_SYMBOL
      }
    }.butNot(letter | digit | space | bracket | delimiter)

    val octalDigit = charOfClass { c =>
      c.isDigit && c != '8' && c != '9'
    }

    val characterLiteral = {
      val charEscape = character('b') | 't' | 'n' | 'f' | 'r' | '"' | ''' | '\\'
      val octEscape = octalDigit ~ octalDigit.atMostNtimes(2)

      ''' ~ ((any.butNot('\\') | ('\\' ~ (charEscape | octEscape)))) ~ '''
    }

    val stringLiteral = {
      val simpleLiteral = '"' ~ (('\\' ~ '"') | charOfClass(c => !c.isControl && c != '"')).zeroOrMore ~ '"'
      val multiLiteral = "\"\"\"" ~ ('"'.atMostNtimes(2) ~ any.butNot('"')).zeroOrMore ~ "\"\"\""

      multiLiteral | simpleLiteral
    }

    val op = opChar.atLeastOnce

    val idrest = (letter | digit).zeroOrMore ~ ('_' ~ op).zeroOrMore

    val varid = charOfClass(_.isLower) ~ idrest

    val plainid = (charOfClass(_.isUpper) ~ idrest) | varid | op

    val symbolLiteral = ''' ~ plainid

    val literalIdentifier = '`' ~ any.butNot('`').atLeastOnce ~ '`'

    val spaces: Movement = space.zeroOrMore
    val comments: Movement = (comment ~ spaces).zeroOrMore
    val commentsAndSpaces: Movement = (spaces ~ comments).zeroOrMore
    val bracketsWithContents = inBrackets('[', ']')
    val curlyBracesWithContents = inBrackets('{', '}')

    implicit def charToMovement(c: Char): SingleCharMovement = character(c)
    implicit def stringToMovement(str: String): Movement  = string(str)
  }

  object MovementHelpers {
    def nextMarker(currentMarker: Int, forward: Boolean): Int = {
      if (forward) currentMarker + 1
      else currentMarker - 1
    }

    def wouldBeDepleted(potentialMarker: Int, sourceWithMarker: SourceWithMarker): Boolean = {
      !sourceWithMarker.isInRange(potentialMarker)
    }

    def doIfNotDepleted(sourceWithMarker: SourceWithMarker)(op: => Option[Int]): Option[Int] = {
      if (sourceWithMarker.isDepleted) None
      else op
    }
  }
}
