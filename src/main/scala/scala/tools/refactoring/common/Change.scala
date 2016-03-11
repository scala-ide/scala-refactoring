/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.util.SourceFile

/**
 * The common interface for all changes.
 */
sealed trait Change

case class TextChange(sourceFile: SourceFile, from: Int, to: Int, text: String) extends Change {

  def file = sourceFile.file

  /**
   * Instead of a change to an existing file, return a change that creates a new file
   * with the change applied to the original file.
   *
   * @param fullNewName The fully qualified package name of the target.
   */
  def toNewFile(fullNewName: String) = {
    val src = Change.applyChanges(List(this), new String(sourceFile.content))
    NewFileChange(fullNewName, src)
  }
}

/**
 * The changes creates a new source file, indicated by the `fullName` parameter. It is of
 * the form "some.package.FileName".
 */
case class NewFileChange(fullName: String, text: String) extends Change

case class RenameSourceFileChange(sourceFile: AbstractFile, to: String) extends Change

object Change {
  /**
   * Applies the list of changes to the source string. NewFileChanges are ignored.
   * Primarily used for testing / debugging.
   */
  def applyChanges(ch: List[Change], source: String): String = {
    val changes = ch collect {
      case tc: TextChange => tc
    }

    val sortedChanges = changes.sortBy { descendingTo }

    /* Test if there are any overlapping text edits. This is
       not necessarily an error, but Eclipse doesn't allow
       overlapping text edits, and this helps us catch them
       in our own tests. */
    sortedChanges.sliding(2).toList foreach {
      case List(TextChange(_, from, _, _), TextChange(_, _, to, _)) =>
        assert(from >= to)
      case _ => ()
    }

    (source /: sortedChanges) { (src, change) =>
      src.substring(0, change.from) + change.text + src.substring(change.to)
    }
  }

  private def descendingTo(change: TextChange) = -change.to

  case class AcceptReject(accepted: List[Change], rejected: List[Change])

  def discardOverlappingChanges(changes: List[Change]): AcceptReject = {
    val applicableChanges = changes.collect {
      case tc: TextChange => tc
    }.sortBy { descendingTo }
    def filter(h: TextChange, rest: List[TextChange], acc: AcceptReject): AcceptReject = rest match {
      case Nil => acc
      case r :: rs if h.from >= r.to =>
        filter(r, rs, acc.copy(accepted = acc.accepted :+ r))
      case r :: rs =>
        filter(h, rs, acc.copy(rejected = acc.rejected :+ r))
    }
    applicableChanges match {
      case h :: tail => filter(h, tail, AcceptReject(h :: Nil, Nil))
      case Nil => AcceptReject(Nil, Nil)
    }
  }
}
