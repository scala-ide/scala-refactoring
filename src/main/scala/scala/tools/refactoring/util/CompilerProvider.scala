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
import scala.reflect.internal.MissingRequirementError

class CompilerInstance {

  lazy val compiler: Global = {

    val settings = new Settings

    // find the jar that has class `className`
    def codeSource(className: String) = Class.forName(className).getProtectionDomain.getCodeSource

    val scalaLibrarySource = codeSource("scala.Unit")

    // is null in Eclipse/OSGI but luckily we don't need it there
    if (scalaLibrarySource != null) {
      val scalaXmlSource = codeSource("scala.xml.Elem")
      val scalaParsingSource = codeSource("scala.util.parsing.combinator.JavaTokenParsers")
      val scalaCompilerSource = codeSource("scala.tools.nsc.Global")

      assert((new File(scalaCompilerSource.getLocation.toURI)).exists, s"File ${scalaCompilerSource.getLocation.toExternalForm} does not exist")

      val libraryJars = List(scalaCompilerSource, scalaLibrarySource, scalaXmlSource, scalaParsingSource).map(_.getLocation.toExternalForm).distinct // distinct to drop duplicate jars in non-modularized Scala verions (as of 2.11.0-M4, xml and util.parsing are in separate jars)

      libraryJars foreach { jarLocation =>
        settings.classpath.append(jarLocation)
        settings.bootclasspath.append(jarLocation)
      }

      settings.processArgumentString("-usejavacp")
    }

    val compiler = new Global(settings, new ConsoleReporter(settings) {
      override def printMessage(pos: Position, msg: String): Unit = {
      }
    })

    try {
      compiler.ask { () =>
        new compiler.Run
      }
    } catch {
      case e: MissingRequirementError =>
        val msg = s"""Could not initialize the compiler!
                     |  ${settings.userSetSettings.mkString("\n  ")}
                     |  ${settings.classpath}
                     |  ${settings.bootclasspath}
                     |  ${settings.javabootclasspath}""".stripMargin
        throw new Exception(msg, e)
    }

    compiler
  }
}

trait TreeCreationMethods {

  val global: scala.tools.nsc.interactive.Global

  protected def treeFromString(src: String, isJava: Boolean = false): global.Tree = {
    val file = new BatchSourceFile(UniqueNames.basename() + (if (isJava) ".java" else ""), src)
    treeFrom(file)
  }

  def treeFrom(src: String): global.Tree = treeFromString(src, false)

  def treeFrom(file: SourceFile): global.Tree = {

    val response = new Response[global.Tree]

    global.ask(() => global.askLoadedTyped(file, true, response))

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

  def parseJava(src: String): global.Tree = treeFromString(src, true)
}

object CompilerInstance extends CompilerInstance

trait CompilerProvider extends TreeCreationMethods {

  val global = CompilerInstance.compiler

  private[refactoring] def resetPresentationCompiler() = global.ask { () =>

    global.unitOfFile.values.foreach { cu =>
      global.removeUnitOf(cu.source)
      assert(global.getUnitOf(cu.source).isEmpty)
    }

    global.askReset

    global.checkNoResponsesOutstanding

    global.reporter.reset()      // Hopefully a fix for https://github.com/scala-ide/scala-refactoring/issues/69
    global.analyzer.resetTyper() // ... added for good measure.
  }
}
