package scala.tools.refactor.tests.utils

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global 
import scala.tools.nsc.reporters.ConsoleReporter 
import scala.tools.nsc.util.BatchSourceFile

trait Compiler {
  
  def error(message: String) = { }
  
  private val settings = new Settings(error)
    
  val compiler = new Global(settings, new ConsoleReporter(settings))
  
  def treeFrom(source: String) = compiler.typedTree(new BatchSourceFile("test", source), false)
}
