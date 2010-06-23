/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile

case class Change(file: AbstractFile, from: Int, to: Int, text: String)

object Change {
  def applyChanges(ch: List[Change], source: String) = {
    
    (source /: ch.sortBy(-_.to)) { (src, change) =>
      src.substring(0, change.from) + change.text + src.substring(change.to)
    }
  }
}