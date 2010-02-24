package scala.tools.refactoring.tests.util

import scala.tools.refactoring.Refactoring

trait TestRefactoring {
  self: Refactoring =>

  def applyChangeSet(ch: ChangeSet, source: String) = {
    source.substring(0, ch.from) + ch.text + source.substring(ch.to)
  }
}
