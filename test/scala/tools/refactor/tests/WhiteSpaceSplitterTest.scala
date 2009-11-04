package scala.tools.refactor.tests

import utils.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

@Test
class WhiteSpaceSplitterTest extends TestCase with TestHelper {
  
  def testAbc {
    treeFrom("class A")
  }

}
