package scala.tools.refactoring.scripts

import scala.tools.refactoring.tests.utils._
import scala.tools.refactoring._
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.transformation._

import scala.tools.nsc.ast._
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

object Path extends CompilerProvider with TreePath {
  
  import global._
  
  def main(args : Array[String]) : Unit = {
    
    val tree = treeFrom("""
      class A {
        def get(i: Int): Int = {
          val a = 1
          val b = a + 1
          b + 1
        }
      }

      class B {
        def get: Int = {
          val x = 5
          x
        }
      }
""")
        
//    val p = tree / ClassDef / 0 / DefDef / -1 / ValDef
    
    val p = tree / DefDef / SubTree("rhs")
    
    println(p)

    // why?
    exit(0)
  }
}
