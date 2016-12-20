package scala.tools.refactoring
package implementations.oimports

import scala.tools.nsc.Global

class ImplicitValDefTraverserPF[G <: Global](val global: G) {
  import global._

  /** Unsupported for Scala 2.10. */
  def apply(traverser: Traverser): PartialFunction[Tree, Unit] = PartialFunction.empty
}
