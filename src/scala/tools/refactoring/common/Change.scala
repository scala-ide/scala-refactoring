package scala.tools.refactoring.common

import scala.tools.nsc.io.AbstractFile

case class Change(file: AbstractFile, from: Int, to: Int, text: String)