package scala.tools.refactoring.scripts

import scala.tools.refactoring.util.LayoutPreferences
import scala.tools.refactoring.util.Tracing
import scala.tools.refactoring.tests.util._
import scala.tools.refactoring._
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.transformation._

import scala.tools.nsc.ast._
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

object Parts2 extends CompilerProvider with Regeneration with LayoutPreferences with Tracing {
  
  def main(args : Array[String]) : Unit = {
    
    val src = """
    import scala.collection.mutable.ListBuffer
    package a {
      object B
    }
"""
    
    val tree = treeFrom(src)
    
    val f = splitIntoFragments(tree)
    
    println(f)
    
    exit(0)
  }
}
