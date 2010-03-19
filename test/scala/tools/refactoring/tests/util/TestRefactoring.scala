package scala.tools.refactoring.tests.util

import scala.tools.refactoring.Refactoring
import scala.tools.refactoring.common.Change
import org.junit.Assert._

trait TestRefactoring {
  
  self: TestHelper =>
   
  abstract class TestRefactoringImpl(source: String, fileName: String) {
  
    val refactoring: Refactoring
    
    def doIt(expected: String, parameters: refactoring.RefactoringParameters) = {
      val result = performRefactoring(parameters)
      assertEquals(expected, applyChangeSet(result, source))
    }
    
    def performRefactoring(parameters: refactoring.RefactoringParameters): List[Change] = {
      val selection = new refactoring.FileSelection(compile(fileName, source), source.indexOf("/*(*/"), source.indexOf("/*)*/"))
      refactoring.prepare(selection) match {
        case Right(prepare) =>
          refactoring.perform(selection, prepare, parameters) match {
            case Right(result) => result
            case Left(error) => fail(error.cause); throw new Exception("Unreachable, :-/ Java type system")
          }
        case Left(error) => fail(error.cause); throw new Exception("Unreachable, :-/ Java type system")
      }
    }
  }
}
