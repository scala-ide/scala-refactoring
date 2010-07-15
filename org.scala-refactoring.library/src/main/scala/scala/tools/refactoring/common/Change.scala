/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile

case class Change(file: AbstractFile, from: Int, to: Int, text: String)

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