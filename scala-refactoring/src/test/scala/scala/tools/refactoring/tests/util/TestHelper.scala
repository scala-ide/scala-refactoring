/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests.util

import scala.tools.nsc.util.BatchSourceFile
import org.junit.Assert._

import scala.tools.refactoring._
import scala.tools.refactoring.common._
import scala.tools.refactoring.transformation._
import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.util.CompilerProvider

trait TestHelper extends CompilerProvider with Transformation with SilentTracing with Selections {
  
  /*
   * A project to test multiple compilation units. Add all 
   * sources using "add" before using any of the lazy vals.
   * */
  abstract class FileSet(val name: String) {
    
    def this() = this("test")
      
    private val srcs = ListBuffer[(String, String)]()
    
    protected def add(src: String, expected: String) = srcs += src → expected
    
    def fileName(src: String) = name +"_"+ sources.indexOf(src).toString
    
    lazy val sources = srcs.unzip._1 toList
    
    lazy val expected = srcs.unzip._2 toList
    
    lazy val trees = sources map (x => compile(fileName(x), x)) map (global.unitOfFile(_).body)
    
    lazy val selection = (sources zip trees flatMap (x => findMarkedNodes(x._1, x._2)) headOption) getOrElse {
      // not all refactorings really need a selection:
      new FileSelection(trees.head.pos.source.file, 0, 0)
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
    
  def findMarkedNodes(src: String, tree: global.Tree) = {
    
    val startPattern = "/*(*/"
    val endPattern = "/*)*/"
    
    val start = src.indexOf(startPattern)
    val end   = src.indexOf(endPattern)
    
    if(start >= 0 && end >= 0)
      Some(new TreeSelection(tree, start + startPattern.length, end))
    else 
      None
  }
}
