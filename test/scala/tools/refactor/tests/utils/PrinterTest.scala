package scala.tools.refactor.tests.utils

import org.junit.Assert._
import java.lang.{StringBuilder => SB}

import scala.tools.refactor.printer._

trait PrinterTest extends Compiler {
  def assert : this.type = this 
  
  def print(src: String) = {
    val buffer = new SB
    val tree = treeFrom(src)
    Printer(buffer, compiler, tree)
    assertEquals(src, buffer.toString)
  }
}