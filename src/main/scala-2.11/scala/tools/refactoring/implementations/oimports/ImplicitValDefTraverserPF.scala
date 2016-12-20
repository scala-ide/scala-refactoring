package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.Global

class ImplicitValDefTraverserPF[G <: Global](val global: G) {
  import global._
  import global.analyzer._

  private def continueWithFunction(traverser: Traverser, rhs: Attachable) = rhs match {
    case TypeApply(fun, _) =>
      traverser.traverse(fun)
  }

  def apply(traverser: Traverser): PartialFunction[Tree, Unit] = {
    case ValDef(_, _, _, rhs: Attachable) if rhs.hasAttachment[MacroExpansionAttachment] =>
      val mea = rhs.attachments.get[MacroExpansionAttachment]
      mea.collect {
        case MacroExpansionAttachment(_, expanded: Typed) =>
          expanded
      }.foreach { expanded =>
        traverser.traverse(expanded.expr)
      }
      continueWithFunction(traverser, rhs)
  }
}
