package scala.tools.refactoring.analysis

import scala.tools.refactoring.util.Selections

trait Analysis extends Indexes with TreeAnalysis {
  self: Selections =>
}
