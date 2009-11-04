package scala.tools.refactor.tests.utils

import org.junit.Assert._
import java.lang.{StringBuilder => SB}

import scala.tools.refactor.printer._

trait PrinterTest extends Compiler {
  def assert : this.type = this 
  
  def print(src: String) = {
    val tree = treeFrom(src)
    val sourceElements = Printer(compiler, tree)
    val buffer = new SB
    sourceElements foreach (_.print(buffer))
    assertEquals(src, buffer.toString)
  }
}