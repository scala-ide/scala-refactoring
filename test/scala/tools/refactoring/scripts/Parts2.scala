package scala.tools.refactoring.scripts

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

object Parts2 extends CompilerProvider {
  
  def main(args : Array[String]) : Unit = {
    
    val src = """
    class A {
      def extractFrom {
        if (true) true
        else {
          try {
            println("huhu")
            if (true) true
            else false
          } catch {
            case _ => ()
          }
        }
      }
    }

"""
    
    val tree = treeFrom(src)
    
    exit(0)
  }
}
