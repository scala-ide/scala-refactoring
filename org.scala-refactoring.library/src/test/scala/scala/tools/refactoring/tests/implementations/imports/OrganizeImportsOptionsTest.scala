/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports
import tests.util.{TestHelper, TestRefactoring}
      
class OrganizeImportsOptionsTest extends OrganizeImportsBaseTest {
  outer =>

  def organize(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(options = List(refactoring.ExpandImports), deps = refactoring.Dependencies.FullyRecompute)
  }.mkChanges  
  
  @Test
  def renamedPackage = new FileSet {
    """
      import java.{ lang => jl, util => ju }
      import ju.{ArrayList => AL}
      trait Y {
        def build(ignored : ju.Map[_, _])
        def build2(ignored : AL[Int])
      }
    """ becomes
    """
      import java.util.{ArrayList => AL}
      import java.{util => ju}
      trait Y {
        def build(ignored : ju.Map[_, _])
        def build2(ignored : AL[Int])
      }
    """
  } applyRefactoring organize
}
