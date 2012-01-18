/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.util

import tools.nsc.util.BatchSourceFile
import tools.nsc.io.AbstractFile
import org.junit.Assert._
import common.Change
import collection.mutable.ListBuffer
import util.CompilerProvider
import scala.tools.refactoring.common.NewFileChange
import scala.tools.refactoring.common.TextChange

trait TestHelper extends ScalaVersionTestRule with Refactoring with CompilerProvider with common.InteractiveScalaCompiler {
  
  type Test = org.junit.Test
  type Ignore = org.junit.Ignore
  type AbstractFile = tools.nsc.io.AbstractFile
  type ConsoleTracing = common.ConsoleTracing
  type SilentTracing = common.SilentTracing
  type GlobalIndexes = analysis.GlobalIndexes
  type ScalaVersion = tests.util.ScalaVersion
    
  /**
   * A project to test multiple compilation units. Add all 
   * sources using "add" before using any of the lazy vals.
   */
  abstract class FileSet(val name: String) {
    
    global.unitOfFile.values.foreach { cu =>
      global.removeUnitOf(cu.source)
      global.getUnitOf(cu.source)
    }
    
    global.askReset
    
    def this() = this(randomFileName())
      
    private val srcs = ListBuffer[(String, String)]()

    implicit def addRefactoringFile(src: String) = new {
      def becomes(expected: String) {
        srcs += src → expected
      }
    }
    
    def fileName(src: String) = name +"_"+ sources.indexOf(src).toString
    
    lazy val sources = srcs.unzip._1 toList
    
    lazy val expected = srcs.unzip._2 toList
    
    lazy val trees = sources map (x => addToCompiler(fileName(x), x)) map (global.unitOfFile(_).body)
    
    lazy val selection = (sources zip trees flatMap (x => findMarkedNodes(x._1, x._2)) headOption) getOrElse {
      // not all refactorings need a selection:
      FileSelection(trees.head.pos.source.file, trees.head, 0, 0)
    }
    
    def apply(f: FileSet => List[String]) = assert(f(this))
    
    val NewFile = ""

    def applyRefactoring(createChanges: FileSet => List[Change]) {
      
      val changes = createChanges(this)
      
      val res = sources zip (sources map fileName) flatMap {
        case (NewFile, name) =>
          changes collect {
            case nfc: NewFileChange => nfc.text
          }

        case (src, name) => 
          val textFileChanges = changes collect {
            case tfc: TextChange if tfc.sourceFile.file.name == name => tfc
          }
          Change.applyChanges(textFileChanges, src) :: Nil

      } filterNot (_.isEmpty)
      
      assert(res)
    }
    
    private def assert(res: List[String]) = {
      assertEquals(srcs.length, res.length)
      expected zip res foreach (p => assertEquals(p._1, p._2))
    }
  }
  
  val startPattern = "/*(*/"
  val endPattern = "/*)*/"
    
  def findMarkedNodes(src: String, tree: global.Tree): Option[Selection] = {
    
    val start = commentSelectionStart(src)
    val end   = commentSelectionEnd(src)
    
    if(start >= 0 && end >= 0)
      Some(FileSelection(tree.pos.source.file, tree, start, end))
    else 
      None
  }

  val emptyAllPositions = topdown(setNoPosition)
  
  def commentSelectionStart(src: String): Int = {
    src.indexOf(startPattern) + startPattern.length
  }
  
  def commentSelectionEnd(src: String): Int = {
    src.indexOf(endPattern)
  }
}
