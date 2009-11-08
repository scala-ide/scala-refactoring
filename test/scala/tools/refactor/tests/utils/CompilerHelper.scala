package scala.tools.refactor.tests.utils

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.BatchSourceFile
import scala.tools.refactor.printer._

private object CompilerInstance {
  
  def error(message: String) = ()

  val settings = new Settings(error)
  
  val scalaObjectSource = Class.forName("scala.ScalaObject").getProtectionDomain.getCodeSource
    
  // is null in Eclipse/OSGI but luckily we don't need it there
  if(scalaObjectSource != null) {
    val compilerPath = Class.forName("scala.tools.nsc.Interpreter").getProtectionDomain.getCodeSource.getLocation
    val libPath = scalaObjectSource.getLocation          
    val origBootclasspath = settings.bootclasspath.value
    val pathList = List(compilerPath,libPath)
    settings.bootclasspath.value = (origBootclasspath :: pathList).mkString(java.io.File.separator)
  }
  
  val compiler = new Global(settings, new ConsoleReporter(settings))
}

trait CompilerProvider extends scala.tools.refactor.Compiler {

  val compiler = CompilerInstance.compiler
  
  def treeFrom(source: String) = {
   compiler.typedTree(new BatchSourceFile("test", source), true)
  }
}
