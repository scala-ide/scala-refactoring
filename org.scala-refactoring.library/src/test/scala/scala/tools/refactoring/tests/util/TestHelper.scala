/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.util

import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.FailedInterrupt
import scala.tools.refactoring.Refactoring
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.NewFileChange
import scala.tools.refactoring.common.TextChange
import scala.tools.refactoring.util.CompilerProvider
import org.junit.Assert.assertEquals
import org.junit.Before
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.common.Selections

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

    def this() = this(randomFileName())
      
    private val srcs = ListBuffer[(String, String)]()

    implicit def addRefactoringFile(src: String) = new {
      def becomes(expected: String) {
        srcs += src → expected
      }
    }
    
    def fileName(src: String) = name +"_"+ sources.indexOf(src).toString
    
    lazy val sources = srcs.unzip._1 toList
    
    def apply(f: FileSet => List[String]) = assert(f(this))
    
    val NewFile = ""

    def applyRefactoring(createChanges: FileSet => List[Change]) {
      
      val changes = try {
        global.ask { () =>
          createChanges(this)
        }
      } catch {
        case e: FailedInterrupt => 
          throw e.getCause
        case e: InterruptedException => 
          throw e.getCause
      }
      
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
      val expected = srcs.unzip._2.toList
      expected zip res foreach (p => assertEquals(p._1, p._2))
    }
  }
  
  def selection(refactoring: Selections with InteractiveScalaCompiler, project: FileSet) = {
    
    val files = project.sources map (x => addToCompiler(project.fileName(x), x))
    val trees: List[refactoring.global.Tree] = files map (refactoring.global.unitOfFile(_).body)
    
    (project.sources zip trees flatMap {
      case (src, tree) => 
        findMarkedNodes(refactoring)(src, tree)
    } headOption) getOrElse {
      refactoring.FileSelection(trees.head.pos.source.file, trees.head, 0, 0)
    }
  }
  
  val startPattern = "/*(*/"
  val endPattern = "/*)*/"
    
  def findMarkedNodes(r: Selections with InteractiveScalaCompiler)(src: String, tree: r.global.Tree): Option[r.Selection] = {
    
    val start = commentSelectionStart(src)
    val end   = commentSelectionEnd(src)
    
    if(start >= 0 && end >= 0)
      Some(r.FileSelection(tree.pos.source.file, tree, start, end))
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
  
  val removeAuxiliaryTrees = ↓(transform {

    case t: global.Tree if (t.pos == global.NoPosition || t.pos.isRange) => t
    
    case t: global.ValDef => global.emptyValDef
    
    // We want to exclude "extends AnyRef" in the pretty printer tests
    case t: global.Select if t.name.isTypeName && t.name.toString != "AnyRef" => t
    
    case t => global.EmptyTree
  })
}
