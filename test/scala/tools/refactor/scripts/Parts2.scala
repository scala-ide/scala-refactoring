package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._

object Parts2 extends Merger with Partitioner {
  
  def main(args : Array[String]) : Unit = {

    import Compiler._
    import compiler._
      
    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
//    val tree = treeFrom("class A/*aa*/(private val i: Int, s: String, a: Any /*a comment for the Any parameter*/)")
    
    val transformer = new Transformer {
      override def transform(tree: Tree): Tree = super.transform(tree) match {
        case Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
        case x => x
      }
    }
    
    val newTree = transformer.transform(tree)
    
    val partitionedOriginal = splitIntoParts(compiler, tree)
    
    println(partitionedOriginal mkString "▒▒")
    
    val partitionedModified = splitIntoParts(compiler, newTree)
    
    println(partitionedModified filter (!_.isWhitespace) mkString " → ")
    
    val merged = merge(partitionedOriginal, partitionedModified filter (!_.isWhitespace))
    
    println(merged mkString "|")
    
    val satisfied = satisfyRequirements(merged)
    
    println(satisfied mkString)
    
    // why?
    exit(0)
  }
}
