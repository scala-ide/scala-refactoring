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

object Parts2 extends Merger with Partitioner with TestTransform with TreeDSL with LayoutHandler with TreePrinter with Tracing with CompilerProvider {

  def main(args : Array[String]) : Unit = {
    

    val l = CODE.LIT("a")
    
    println(l)
    
    
//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
//      val tree = treeFrom("class A")
//    val tree = treeFrom("""
//      class A {
//        val b: String //b-string
//        def c: Unit = {
//          val x = 
//            "abc" //assign
//              println(x)
//        }
//      }
//""")
//    val res = insertValue.transform(tree)
        
        
//    val partitionedOriginal = splitIntoFragments(tree)
//    
//    println(partitionedOriginal)
//    println("===========")
//
//    val ess = essentialFragments(tree, new FragmentRepository(partitionedOriginal))
//
//    println(ess)
//    println("===========")
//
//    
//    val newTree = newMethod.transform(tree)
//    //val newTree = insertValue.transform(tree)
////    val newTree = reverseClassParameters.transform(tree)
//    val partitionedModified = essentialFragments(newTree, new FragmentRepository(partitionedOriginal))
//    
//    println("Modified: "+ partitionedModified)
//    
//    println("===========")
//    val merged = merge(partitionedModified, new FragmentRepository(partitionedOriginal))
//    println(merged map (_.print) mkString "")
// 
    // why?
    exit(0)
  }
}
