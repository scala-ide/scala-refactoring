package scala.tools.refactoring.tests.util

import scala.tools.refactoring.Refactoring
import org.junit.Assert._

trait TestRefactoring {
  
  self: TestHelper =>
  
  abstract class TestRefactoringImpl(source: String) {
  
    val refactoring: Refactoring
    
    def doIt(expected: String, parameters: refactoring.RefactoringParameters) = {
      val selection = new refactoring.FileSelection(compile(source), source.indexOf("/*(*/"), source.indexOf("/*)*/"))
      refactoring.prepare(selection) match {
        case Right(prepare) =>
          val result = refactoring.perform(selection, prepare, parameters) match {
            case Right(result) => applyChangeSet(result, source)
            case Left(error) => fail(error.cause)
          }
          assertEquals(expected, result)
        case Left(error) => fail(error.cause)
      }
    }
  
    def applyChangeSet(ch: refactoring.ChangeSet, source: String) = {
      
      val descending: (refactoring.Change, refactoring.Change) => Boolean = _.to > _.to
      
      (source /: ch.sortWith(descending)) { (src, ch) =>
        src.substring(0, ch.from) + ch.text + src.substring(ch.to)
      }
    }
  }
}
