package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._

object PartsPrinter extends Partitioner {
  
  def main(args : Array[String]) : Unit = {
    
    import Compiler._
    import compiler._
      
//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
    val tree = treeFrom("class A(i: Int, s: String)")
    
    val transformer = new Transformer {
      override def transform(tree: Tree): Tree = super.transform(tree) match {
        case Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
        case x => x
      }
    }
    
    val newTree = transformer.transform(tree)
    
    val partitionedOriginal = splitIntoParts(compiler, tree)
    
    println(""" 
digraph structs {
  rankdir = LR;          
  ranksep = 0.2;
  edge[arrowsize=1, weight=100, arrowhead=none];
  node[shape=record, fontname="Courier", margin=0.05, height=0.4, width=0]
    """)
    
    println(partitionedOriginal map {
      case se: WhitespacePart => "  "+ se.hashCode +"[label=\""+ se +"\"];"
      case se: SymTreePart    => "  "+ se.hashCode +"[label=\""+ se +"\", style=filled, fillcolor=lightgrey];"
      case se: FlagPart       => "  "+ se.hashCode +"[label=\""+ se +"\", style=filled, fillcolor=lightgrey];"
      case se: StringPart     => ()
      case se: BeginOfFile    => "  "+ se.hashCode +"[label=\"BOF\", style=filled, fillcolor=lightgrey];"
      case se: EndOfFile      => "  "+ se.hashCode +"[label=\"EOF\", style=filled, fillcolor=lightgrey];"
      case     NullPart       => ()
    } mkString "\n")
    
    println((partitionedOriginal map (_.hashCode) mkString " -> ") + ";")
    
    println((splitIntoParts(compiler, newTree) filter (!_.isWhitespace) map (_.hashCode) mkString " -> ") + "[weight=1, arrowhead=normal]")
    
    println("\n}")
    
    // why?
    exit(0)
  }
}
