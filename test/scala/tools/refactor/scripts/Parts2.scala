package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

import scala.tools.nsc.ast._
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position

object Parts2 extends Merger with Partitioner with TestTransform with CompilerProvider with TreeDSL with WhitespaceHandler with TreePrinter {
  
  val global = compiler
  import CODE._
  import global._          

  def main(args : Array[String]) : Unit = {
    
    
//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
//      val tree = treeFrom("class A")
    val tree = treeFrom("""
    class Xyz(private val abc: String, var int: Int) {

        }
""")
//    val res = insertValue.transform(tree)
        
        
    val partitionedOriginal = splitIntoFragments(tree)
    
    println(partitionedOriginal)
    println("===========")

    val ess = essentialFragments(tree, new FragmentRepository(partitionedOriginal))

    println(ess)
    println("===========")

    
    val newTree = bodyInBody.transform(tree)
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
