/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.common

import scala.tools.nsc.io.AbstractFile

case class Change(file: AbstractFile, from: Int, to: Int, text: String)

object Change {
  def applyChanges(ch: List[Change], source: String) = {
  
    val descending: (Change, Change) => Boolean = _.to > _.to
    
    (source /: ch.sortWith(descending)) { (src, ch) =>
      src.substring(0, ch.from) + ch.text + src.substring(ch.to)
    }
  }
}