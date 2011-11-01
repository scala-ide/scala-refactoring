/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile
import scala.tools.nsc.util.SourceFile

case class Change(file: AbstractFile, from: Int, to: Int, text: String) {
  // replace the `file` with this one in the next version
  val underlyingSource = None: Option[SourceFile]
}

object Change {
  
  /**
   * Applies the list of changes to the source string.
   */
  def applyChanges(ch: List[Change], source: String): String = {
    (source /: ch.sortBy(-_.to)) { (src, change) =>
      src.substring(0, change.from) + change.text + src.substring(change.to)
    }
  }
}
