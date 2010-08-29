package scala.tools.refactoring
package tests.transformation

import org.junit._
import org.junit.Assert._
import transformation._
import PartialFunction.condOpt

@Test
class NotTest extends Transformations {

  @Test
  def notTest {
    val t = transformation[String, String] { case "A" => "B" }

    assertEquals(None, not(t)("A"))
    assertEquals(Some("X"), not(t)("X"))
  }
}