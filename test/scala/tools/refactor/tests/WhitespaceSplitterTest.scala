package scala.tools.refactor.tests

import utils.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactor.printer._

@Test
class WhitespaceSplitterTest extends TestCase with TestHelper {

  def testClassParameters() = {
    "class A ( i: /*c*/Int, s: String)"     splitsInto "class ▒A (▒ i: /*c*/▒Int▒s: ▒String▒)"
    "class A(i: Int, s: String, f: Float)"  splitsInto "class ▒A(▒i: ▒Int▒s: ▒String▒f: ▒Float▒)"
    "class A(/*->*/i: Int/*<-*/)"           splitsInto "class ▒A(▒/*->*/i: ▒Int/*<-*/▒)"
  }
}

