/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package util

import java.io.File
import scala.tools.nsc.interactive.{Response, Global}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, BatchSourceFile}
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter

class CompilerInstance {
  
  def additionalClassPathEntry: Option[String] = None
  
  lazy val compiler = {
    
    val settings = new Settings
    
    val scalaObjectSource = Class.forName("scala.Unit").getProtectionDomain.getCodeSource
      
    // is null in Eclipse/OSGI but luckily we don't need it there
    if(scalaObjectSource != null) {
      val compilerPath = Class.forName("scala.tools.nsc.Interpreter").getProtectionDomain.getCodeSource.getLocation
      val libPath = scalaObjectSource.getLocation          
      val pathList = List(compilerPath,libPath)
      val origBootclasspath = settings.bootclasspath.value
      settings.bootclasspath.value = ((origBootclasspath :: pathList) ::: additionalClassPathEntry.toList) mkString File.pathSeparator
    }
    
    val compiler = new Global(settings, new ConsoleReporter(settings) {
      override def printMessage(pos: Position, msg: String) {
        //throw new Exception(pos.source.file.name + pos.show + msg)
      }
    })
  
    new compiler.Run  
    
    compiler
  }
}

trait TreeCreationMethods {
  
  private def isScala(version: String) = scala.util.Properties.versionString.contains(version)
    
  val global: scala.tools.nsc.interactive.Global
  
  val randomFileName = {
    val r = new java.util.Random
    () => "file"+ r.nextInt
  }
    
  def treeFrom(src: String): global.Tree = {
    val file = new BatchSourceFile(randomFileName(), src)
    treeFrom(file)
  }
  
  def treeFrom(file: SourceFile): global.Tree = {
        
    val r1 = new Response[global.Tree]
    global.askParsedEntered(file, true, r1)
    r1.get // we don't care about the result yet
    
    val r2 = new Response[global.Tree]
    global.askType(file, false, r2)
    r2.get match {
      case Left(tree) => tree
      case Right(ex) => throw ex
    }
  }

  def treesFrom(sources: List[SourceFile]): List[global.Tree] =
    sources map treeFrom
  
  /**
   * Add a source file with the given name and content to this compiler instance.
   * 
   * @param name the name of the file; adding different files with the same name can lead to problems
   */
  def addToCompiler(name: String, src: String): AbstractFile = {
    val file = new BatchSourceFile(name, src)
    treeFrom(file) // use the side effect
    file.file
  } 
}

object CompilerInstance extends CompilerInstance

trait CompilerProvider extends TreeCreationMethods {

  val global = CompilerInstance.compiler
}
