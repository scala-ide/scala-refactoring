package scala.tools.refactor.tests

import utils._

import scala.tools.refactor.printer._

import junit.framework._

class First extends TestCase with PrinterTest {
  
  def testOne() = assert print
    """
    		object A
    """
  
}