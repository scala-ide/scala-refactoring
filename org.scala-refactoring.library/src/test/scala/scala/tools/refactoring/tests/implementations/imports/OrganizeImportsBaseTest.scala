/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports
import tests.util.TestHelper
import tests.util.TestRefactoring

abstract class OrganizeImportsBaseTest extends TestHelper with TestRefactoring {

  abstract class OrganizeImportsRefatoring(pro: FileSet) extends TestRefactoringImpl(pro) {
    val refactoring = new OrganizeImports with SilentTracing { val global = OrganizeImportsBaseTest.this.global }
    type RefactoringParameters = refactoring.RefactoringParameters
    val params: RefactoringParameters
    def mkChanges = performRefactoring(params)
  }
}
