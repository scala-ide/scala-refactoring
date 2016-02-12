package scala.tools.refactoring
package implementations

trait ClassDefOrganizeImports { self: OrganizeImports =>
  import global._

  def organizeImportsInClassDefs(tree: Tree): Tree = tree
}
