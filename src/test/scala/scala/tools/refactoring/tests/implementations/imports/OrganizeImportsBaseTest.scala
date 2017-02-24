/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports
import sourcegen.Formatting
import tests.util.TestHelper
import tests.util.TestRefactoring

abstract class OrganizeImportsBaseTest extends TestHelper with TestRefactoring {

  abstract class OrganizeImportsRefatoring(pro: FileSet, formatting: Formatting = new Formatting{}) extends TestRefactoringImpl(pro) {
    val refactoring = new OrganizeImports {
      val global = OrganizeImportsBaseTest.this.global
      override val dropScalaPackage = formatting.dropScalaPackage
      override val lineDelimiter = formatting.lineDelimiter
    }
    type RefactoringParameters = refactoring.RefactoringParameters
    val params: RefactoringParameters
    def mkChanges = performRefactoring(params)
  }
}
