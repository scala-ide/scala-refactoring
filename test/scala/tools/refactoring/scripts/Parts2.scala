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

object Parts2 extends TestHelper with TestTransform {

  def main(args : Array[String]) : Unit = {
    
    val src = """
      class A {
        def get(i: Int): Int = {
          val a = 1
/*(*/     val b = a + i    /*)*/
          b
        }
      }
"""
    
    val tree = treeFrom(src)

    val partitionedOriginal = splitIntoFragments(tree)
    
    println(partitionedOriginal)
    println("===========")

    val ess = essentialFragments(tree, new FragmentRepository(partitionedOriginal))

    println(ess)
    println("===========")
    
    val selection = findMarkedNodes(src, tree)
    val index = new DeclarationIndex
    index.processTree(tree)
    
    val selectedMethod = tree find {
      // what happens with nested defs? should we use filter and take the last (== smallest) one?
      case t: global.DefDef if selection isContainedIn t => true
      case _ => false
    } match {
      case Some(tree: global.DefDef) => tree
      case None => throw new Exception("no enclosing defdef found")
    }
    
    val parameters = inboundLocalDependencies(index, selection, selectedMethod.symbol) 
    

    
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
