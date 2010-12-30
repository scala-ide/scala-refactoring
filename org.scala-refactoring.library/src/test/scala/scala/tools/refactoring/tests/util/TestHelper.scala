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

trait TestHelper extends Refactoring with CompilerProvider with common.InteractiveScalaCompiler {
  
  type Test = org.junit.Test
  type AbstractFile = tools.nsc.io.AbstractFile
  type ConsoleTracing = common.ConsoleTracing
  type SilentTracing = common.SilentTracing
  type GlobalIndexes = analysis.GlobalIndexes
  
  /**
   * A project to test multiple compilation units. Add all 
   * sources using "add" before using any of the lazy vals.
   */
  abstract class FileSet(val name: String) {
    
    def this() = this("test")
      
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
      FileSelection(trees.head.pos.source.file, 0, 0)
    }
    
    def apply(f: FileSet => List[String]) = assert(f(this))
    
    def applyRefactoring(createChanges: FileSet => List[Change]) {
      
      val changes = createChanges(this)
      
      val res = sources zip (sources map fileName) map {
        case (src, name) => 
          val changeSet = changes filter (_.file.name == name)
          Change.applyChanges(changeSet, src)
      }
      
      assert(res)
    }
    
    private def assert(res: List[String]) = {
      assertEquals(srcs.length, res.length)
      expected zip res foreach (p => assertEquals(p._1, p._2))
    }
  }
  
  val startPattern = "/*(*/"
  val endPattern = "/*)*/"
    
  def findMarkedNodes(src: String, tree: global.Tree) = {
    
    val start = src.indexOf(startPattern)
    val end   = src.indexOf(endPattern)
    
    if(start >= 0 && end >= 0)
      Some(FileSelection(tree.pos.source.file, start + startPattern.length, end))
    else 
      None
  }

  val noPosition = transform {
    case t: global.Tree => t.pos = global.NoPosition; t
  }

  val emptyAllPositions = topdown(noPosition)
}
