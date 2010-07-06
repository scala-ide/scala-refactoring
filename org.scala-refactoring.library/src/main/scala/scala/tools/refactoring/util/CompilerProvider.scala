/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package util

import java.io.File.pathSeparator
import tools.nsc.util.Position
import tools.nsc.io.AbstractFile
import tools.nsc.Settings
import tools.nsc.interactive.Global
import tools.nsc.reporters.ConsoleReporter
import tools.nsc.util.BatchSourceFile
import tools.nsc.util.SourceFile

class CompilerInstance {
  
  def additionalClassPathEntry: Option[String] = None
  
  lazy val compiler = {
    
    val settings = new Settings
    
    val scalaObjectSource = Class.forName("scala.ScalaObject").getProtectionDomain.getCodeSource
      
    // is null in Eclipse/OSGI but luckily we don't need it there
    if(scalaObjectSource != null) {
      val compilerPath = Class.forName("scala.tools.nsc.Interpreter").getProtectionDomain.getCodeSource.getLocation
      val libPath = scalaObjectSource.getLocation          
      val pathList = List(compilerPath,libPath)
      val origBootclasspath = settings.bootclasspath.value
      settings.bootclasspath.value = ((origBootclasspath :: pathList) ::: additionalClassPathEntry.toList) mkString pathSeparator
    }
    
    val compiler = new Global(settings, new ConsoleReporter(settings) {
      override def printMessage(pos: Position, msg: String) {
        throw new Exception(pos.source.file.name + pos.show + msg)
      }
    })
  
    new compiler.Run  
    
    compiler
  }
}

trait TreeCreationMethods {
  
  val global: scala.tools.nsc.interactive.Global
    
  def treeFrom(src: String): global.Tree = 
    global.typedTree(new BatchSourceFile("testFile", src), true)
  
  def treeFrom(source: SourceFile, forceReload: Boolean): global.Tree = 
    global.typedTree(source, forceReload)

  def treesFrom(sources: List[SourceFile], forceReload: Boolean): List[global.Tree] =
    sources map ( treeFrom(_, forceReload) )
  
  def addToCompiler(name: String, src: String): AbstractFile = {
    val file = new BatchSourceFile(name, src)
    global.typedTree(file, true)
    file.file
  } 
}

object CompilerInstance extends CompilerInstance

trait CompilerProvider extends TreeCreationMethods {

  val global = CompilerInstance.compiler
}


