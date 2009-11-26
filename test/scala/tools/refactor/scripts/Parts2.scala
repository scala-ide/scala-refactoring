package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

import scala.tools.nsc.ast._
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position

object Parts2 extends Merger with Partitioner with TestTransform with CompilerProvider with TreeDSL with WhitespaceHandler with TreePrinter {
  
  val global = compiler
          
  def main(args : Array[String]) : Unit = {
    
    import CODE._
    import global._
    
//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
//      val tree = treeFrom("class A")
    val tree = treeFrom("""
      class A {
        val b: String //b-string
        def c: Unit = {
          def d: Int = {
            5
          } // after d
          d
        } // after c
      } // after A
""")
//    val res = insertValue.transform(tree)
        
        
    val partitionedOriginal = splitIntoParts(tree)
    
    println(partitionedOriginal)
    println("===========")

    val ess = essentialParts(tree, new PartsHolder(partitionedOriginal))

    println(ess)
    println("===========")

    
    val newTree = bodyInBody.transform(tree)
    //val newTree = insertValue.transform(tree)
//    val newTree = reverseClassParameters.transform(tree)
    val partitionedModified = essentialParts(newTree, new PartsHolder(partitionedOriginal))
    
    println("Modified: "+ partitionedModified)
    
    println("===========")
    val merged = merge(partitionedModified, new PartsHolder(partitionedOriginal))
    println(merged map (_.print) mkString "")
 
    // why?
    exit(0)
  }
}
