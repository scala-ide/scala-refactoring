package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils.Compiler
import scala.tools.refactor.printer._

object SourceElements extends Compiler {
  
  def main(args : Array[String]) : Unit = {
      
    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
    
    import compiler._
    
    val transformer = new Transformer {
      override def transform(tree: Tree): Tree = super.transform(tree) match {
        case Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
        case x => x
      }
    }
    
    val newTree = transformer.transform(tree)
    
    println(Printer(compiler, tree) map {
      case se: WhiteSpacePart => "["+se+"]"
      case se: SymbolPart       => "{"+se+"}"
      case se: FlagPart       => "{"+se+"}"
    } mkString " -> ")    
    
    println(Printer(compiler, newTree) map {
      case se: WhiteSpacePart => ;
      case se: SymbolPart       => "{"+se+"}"
      case se: FlagPart       => "{"+se+"}"
    } mkString " -> ")
    
    //println(Printer(compiler, tree) mkString "")
  }
}
