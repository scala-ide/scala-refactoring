/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package util

import java.io.File
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.Position

class CompilerInstance {

  def additionalClassPathEntry: Option[String] = None

  lazy val compiler = {

    val settings = new Settings

    // find the jar that has class `className`
    def codeSource(className: String) = Class.forName(className).getProtectionDomain.getCodeSource

    val scalaObjectSource  = codeSource("scala.Unit")

    // is null in Eclipse/OSGI but luckily we don't need it there
    if(scalaObjectSource != null) {
      val scalaXmlSource     = codeSource("scala.xml.Elem")
      val scalaParsingSource = codeSource("scala.util.parsing.combinator.JavaTokenParsers")
      val compilerSource     = codeSource("scala.tools.nsc.Interpreter")

      val libraryJars = List(compilerSource, scalaObjectSource, scalaXmlSource, scalaParsingSource).map(_.getLocation).distinct // distinct to drop duplicate jars in non-modularized Scala verions (as of 2.11.0-M4, xml and util.parsing are in separate jars)
      val origBootclasspath = settings.bootclasspath.value
      settings.bootclasspath.value = ((origBootclasspath :: libraryJars) ::: additionalClassPathEntry.toList) mkString File.pathSeparator
    }

    val compiler = new Global(settings, new ConsoleReporter(settings) {
      override def printMessage(pos: Position, msg: String) {
        //throw new Exception(pos.source.file.name + pos.show + msg)
      }
    })

    compiler.ask { () =>
      new compiler.Run
    }

    compiler
  }
}

trait TreeCreationMethods {

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

    val response = new Response[global.Tree]

    global.ask(() => global.askLoadedTyped(file, response))

    response.get match {
      case Left(tree) => tree
      case Right(ex) => throw ex
    }
  }

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

  private [refactoring] def resetPresentationCompiler() {

    global.unitOfFile.values.foreach { cu =>
      global.removeUnitOf(cu.source)
      assert(global.getUnitOf(cu.source).isEmpty)
    }

    global.askReset

    global.checkNoResponsesOutstanding
  }
}
