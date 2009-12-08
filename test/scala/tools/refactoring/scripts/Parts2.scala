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

object Parts2 extends Merger with Partitioner with TestTransform with LayoutHandler with TreePrinter with Tracing with CompilerProvider {

  def main(args : Array[String]) : Unit = {
    
    val tree = treeFrom("""
      class A {
        def get: Int = {
          val a = 1
          val b = a + 1
          b + 1
        }
      }
""")
        
    val partitionedOriginal = splitIntoFragments(tree)
    
    println(partitionedOriginal)
    println("===========")

    val ess = essentialFragments(tree, new FragmentRepository(partitionedOriginal))

    println(ess)
    println("===========")

    
    val newTree = newMethod.transform(tree)
    //val newTree = insertValue.transform(tree)
//    val newTree = reverseClassParameters.transform(tree)
    val partitionedModified = essentialFragments(newTree, new FragmentRepository(partitionedOriginal))
    
    println("Modified: "+ partitionedModified)
    
    println("===========")
    val merged = merge(partitionedModified, new FragmentRepository(partitionedOriginal))
    println(merged map (_.print) mkString "")
 
    // why?
    exit(0)
  }
}
