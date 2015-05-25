package scala.tools.refactoring.util

import scala.annotation.tailrec
import java.util.Arrays
import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

/**
 * Represents source code with a movable marker.
 *
 * @see [[SourceWithMarker.Movement]]
 * @see [[SourceWithMarker.Movements]]
 */
final case class SourceWithMarker(source: Array[Char] = Array(), marker: Int = 0) {
  import SourceWithMarker._

  assertLegalState()

  def moveMarker(movement: Movement): SourceWithMarker = {
    if (isDepleted) this
    else movement(this).map(m => copy(marker = m)).getOrElse(this)
  }

  def current: Char = source(marker)

  def isDepleted: Boolean = {
    marker < 0 || marker >= source.length
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

  private def assertLegalState() {
    require(marker >= -1 && marker <= source.length, s"Marker out of bounds: $marker")
  }
}

object SourceWithMarker {

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
   * scala> val = srcAtx = src.moveMarker(movement)
   * srcAtx: scala.tools.refactoring.util.SourceWithMarker = ... <x> = ...
   * scala> val moveBackToVal = ("al" ~ commentsAndSpaces ~ "x").backward
   * scala> val srcAtVal = srcAtx.moveMarker(moveBackToVal)
   * srcAtVal:  scala.tools.refactoring.util.SourceWithMarker = ...te <v>al ...
   * }}}
   *
   * @see [[Movements]]
   */
  trait Movement { outer =>
    def apply(sourceWithMarker: SourceWithMarker): Option[Int]
    def backward: Movement

    final def ~(other: Movement): Movement = new Movement { inner =>
      override def apply(sourceWithMarker: SourceWithMarker): Option[Int] = {
        outer(sourceWithMarker).map(newMarker => sourceWithMarker.copy(marker = newMarker)).flatMap(other.apply)
      }

      override def backward = new Movement  {
        override def apply(sourceWithMarker: SourceWithMarker): Option[Int] = {
         other.backward.apply(sourceWithMarker).map(newMarker => sourceWithMarker.copy(marker = newMarker)).flatMap(outer.backward.apply)
        }

        override def backward = inner
      }
    }

    final def |(other: Movement): Movement = new Movement { inner =>
      override def apply(sourceWithMarker: SourceWithMarker): Option[Int] = {
        outer(sourceWithMarker).orElse(other(sourceWithMarker))
      }

      override def backward = new Movement  {
        override def apply(sourceWithMarker: SourceWithMarker): Option[Int] = {
          (other.backward(sourceWithMarker)).orElse(outer.backward(sourceWithMarker))
        }

        override def backward = inner
      }
    }

    final def zeroOrMore: Movement = {
      class Repeat(forward: Boolean = true) extends Movement {
        override def apply(sourceWithMarker: SourceWithMarker): Option[Int] = {
          val mvmt = if (forward) outer else outer.backward

          @tailrec
          def go(sourceWithMarker: SourceWithMarker): Option[Int] = {
            if (sourceWithMarker.isDepleted) {
              Some(sourceWithMarker.marker)
            } else {
              mvmt(sourceWithMarker) match {
                case Some(marker) =>
                  if (marker != sourceWithMarker.marker) go(sourceWithMarker.copy(marker = marker))
                  else Some(marker)
                case None => Some(sourceWithMarker.marker)
              }
            }
          }

          go(sourceWithMarker)
        }

        override def backward: Movement = new Repeat(!forward)
      }
      new Repeat
    }

    final def atLeastOnce: Movement = {
      this ~ this.zeroOrMore
    }
  }

  object Movements {
    import MovementHelpers._

    final class ConsumeChar(c: Char, forward: Boolean = true) extends Movement {
      override def apply(sourceWithMarker: SourceWithMarker) = {
        if (sourceWithMarker.current == c) Some(nextMarker(sourceWithMarker.marker, forward))
        else None
      }

      override def backward = new ConsumeChar(c, !forward)
    }

    final class ConsumeString(str: String, forward: Boolean = true) extends Movement {
      override def apply(sourceWithMarker: SourceWithMarker) = {
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

      override def backward = new ConsumeString(str, !forward)
    }

    final class ConsumeSpace(forward: Boolean = true) extends Movement {
      override def apply(sourceWithMarker: SourceWithMarker) = {
        if (sourceWithMarker.current.isWhitespace) Some(nextMarker(sourceWithMarker.marker, forward))
        else None
      }

      override def backward = new ConsumeSpace(!forward)
    }

    final class ConsumeComment(forward: Boolean = true) extends Movement {
      override def apply(sourceWithMarker: SourceWithMarker) = {

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

        if(!forward) go(sourceWithMarker.marker).orElse(go(sourceWithMarker.marker, inSingleLineComment = true))
        else go(sourceWithMarker.marker)
      }

      override def backward = new ConsumeComment(!forward)
    }

    final class ConsumeInBrackets(open: Char, close: Char, forward: Boolean = true) extends Movement {
      override def apply(sourceWithMarker: SourceWithMarker) = {
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

      override def backward = new ConsumeInBrackets(open, close, !forward)
    }

    val space: Movement = new ConsumeSpace()
    val spaces: Movement = new ConsumeSpace().zeroOrMore
    val comments: Movement = (new ConsumeComment() ~ spaces).zeroOrMore
    val commentsAndSpaces: Movement = (spaces ~ comments).zeroOrMore
    val bracketsWithContents: Movement = new ConsumeInBrackets('[', ']')

    implicit def charToMovement(c: Char): ConsumeChar = new ConsumeChar(c)
    implicit def stringToMovement(str: String): ConsumeString = new ConsumeString(str)
  }

  object MovementHelpers {
    def nextMarker(currentMarker: Int, forward: Boolean): Int = {
      if (forward) currentMarker + 1
      else currentMarker - 1
    }

    def wouldBeDepleted(potentialMarker: Int, sourceWithMarker: SourceWithMarker): Boolean = {
      if (potentialMarker < 0) true
      else if (potentialMarker >= sourceWithMarker.source.length) true
      else false
    }
  }
}
