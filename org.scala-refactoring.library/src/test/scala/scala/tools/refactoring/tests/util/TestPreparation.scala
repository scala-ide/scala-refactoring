package scala.tools.refactoring
package tests.util

import org.junit.Assert._

trait TestPreparation extends TestRefactoring{

  implicit def convertToFileSet(s: String): FileSet = new FileSet{s becomes ""}

  abstract class Preparation(content: FileSet) extends TestRefactoringImpl(content) {
    def assertSuccess: this.type = {
      assertTrue(preparationResult.isRight)
      this
    }
    def assertFailure: this.type = {
      assertTrue(preparationResult.isLeft)
      this
    }
    def done = ()
  }
}