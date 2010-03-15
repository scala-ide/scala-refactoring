package scala.tools.refactoring.regeneration

import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.LayoutPreferences

trait Regeneration extends LayoutHandler with Merger with Partitioner with TreePrinter with SourceHelper with Fragments with FragmentRepository {
  self: LayoutPreferences with Tracing =>
}
