package scala.tools.refactoring
package implementations.oimports

import implementations.OrganizeImports

class ImplicitValDefTraverserPF[O <: OrganizeImports](val oiInstance: O) {
  import oiInstance.global._

  /** Unsupported for Scala 2.10. */
  def apply(traverser: Traverser): PartialFunction[Tree, Unit] = PartialFunction.empty
}
