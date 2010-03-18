package scala.tools.refactoring.tests.util

import scala.tools.nsc.util.Position
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.BatchSourceFile
import scala.tools.refactoring.regeneration._

private object CompilerInstance {
  
  val settings = new Settings
  
  val scalaObjectSource = Class.forName("scala.ScalaObject").getProtectionDomain.getCodeSource
    
  // is null in Eclipse/OSGI but luckily we don't need it there
  if(scalaObjectSource != null) {
    val compilerPath = Class.forName("scala.tools.nsc.Interpreter").getProtectionDomain.getCodeSource.getLocation
    val libPath = scalaObjectSource.getLocation          
    val pathList = List(compilerPath,libPath)
	  val origBootclasspath = settings.bootclasspath.value
    settings.bootclasspath.value = (origBootclasspath :: pathList).mkString(java.io.File.separator)
  }
  
  val compiler = new Global(settings, new ConsoleReporter(settings) {
    override def printMessage(pos: Position, msg: String) {
      throw new Exception(pos.source.file.name + pos.show + msg)
    }
  })
  
  new compiler.Run
}

trait CompilerProvider {

  val global = CompilerInstance.compiler
    
  def treeFrom(source: String) = {
    global.typedTree(new BatchSourceFile("test", source), true)
  }
  
  def compile(source: String): AbstractFile = {
    val file = new BatchSourceFile("test", source)
    global.typedTree(file, true)
    file.file
  }
}
