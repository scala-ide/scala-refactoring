package scala.tools.refactoring.tests.util

import scala.tools.refactoring.util.Casts._
import org.junit.Test
import org.junit.Assert._

class TestCasts {
  @Test
  def tryMatchWithSimpleExamples(): Unit = {
    assertEquals(None, 
      3.tryMatch {
        case 4 => "Never!"
    })
    
    assertEquals(Some(42),
        "42".tryMatch {
          case "42" => 42
      })
  }
}