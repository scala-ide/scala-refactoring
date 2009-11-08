package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

object Parts2 extends Merger with Partitioner with Transform with CompilerProvider {

  def main(args : Array[String]) : Unit = {

//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
    val tree = treeFrom("class A(i: Int, s: String)")
    
    val newTree = reverseClassParameters.transform(tree)
    
    val partitionedOriginal = splitIntoParts(tree)
    
    println(partitionedOriginal mkString "▒▒")
    
    val partitionedModified = splitIntoParts(newTree)
    
    println(partitionedModified filter (!_.isWhitespace) mkString " → ")
    
    val merged = merge(partitionedOriginal, partitionedModified filter (!_.isWhitespace))
    
    println(merged mkString "|")
    
    val satisfied = satisfyRequirements(merged)
    
    println(satisfied mkString)
    
    // why?
    exit(0)
  }
}
