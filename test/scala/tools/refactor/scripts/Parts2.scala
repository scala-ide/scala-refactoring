package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

import scala.tools.nsc.ast._
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position

object Parts2 extends Merger with Partitioner with Transform with CompilerProvider with TreeDSL with WhitespaceSplitter with TreePrinter {
  
  val global = compiler
          
  def main(args : Array[String]) : Unit = {
    
    import CODE._
    import global._
    
//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
//      val tree = treeFrom("class A")
    val tree = treeFrom("""
class Abc {
  /*test*/ def abc(i: Int) = {
    42
  }
  /*test2*/ def b = {
    println("hello")
    5
  }
}
        """)

//    val res = insertValue.transform(tree)
//
    val ess = essentialParts(tree)

    println(ess)
    println("===========")

    val partitionedOriginal = splitIntoParts(tree)
    
    println(partitionedOriginal.children)
    println("===========")
    
    val newTree = insertValue.transform(tree)
    //val newTree = reverseClassParameters.transform(tree)
    val partitionedModified = essentialParts(newTree)
    
    println("Modified: "+ partitionedModified)
    
    println("===========")
    println(merge(partitionedModified, partitionedOriginal) map (_.print) mkString "")
 
    // why?
    exit(0)
  }
}
