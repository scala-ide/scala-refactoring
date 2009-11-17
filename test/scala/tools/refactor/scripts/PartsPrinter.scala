package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

object PartsPrinter extends Partitioner with CompilerProvider with Transform with Merger {
  
  def m(partsWithWs: CompositePart, part: CompositePart) {
    
    val partsHolder = new PartsHolder(partsWithWs)
    
    def id(part: Part) = part match {
      case part: OriginalSourcePart => ""+ part.start +"999"+ part.end
      case _ => "?"
    }
  
    def innerMerge(part: CompositePart): Unit = {
      
      def escape(s: String) = s.replace("\n", "\\\\n").replace("{", "\\{").replace("}", "\\}")
      
      val currentParent = id(part)
        
      val wsBefore = splitWhitespaceBetween(partsHolder getPrevious part)._2
      val wsAfter  = splitWhitespaceBetween(partsHolder getNext part)._1
    
      println("  "+ currentParent +"[label=\""+ escape(wsBefore) +"|<1>"+ part.tree.getClass.toString.split("\\$").last +"|"+ escape(wsAfter) +"\"];")
      
      part.children  foreach {
        case current: CompositePart => 
          innerMerge(current)
          println("  "+ currentParent +":1 -> "+ id(current)) +":1"
        case current: CompositePart#BeginOfScope =>
          val wsAfter  = splitWhitespaceBetween(partsHolder getNext current)._1
    
          println("  "+ id(current) +"[label=\""+ current.toString +"|"+ escape(wsAfter) +"\"];")
          println("  "+ currentParent +" -> "+ id(current))
        case current: CompositePart#EndOfScope =>
          val wsBefore = splitWhitespaceBetween(partsHolder getPrevious current)._2
    
          println("  "+ id(current) +"[label=\""+ escape(wsBefore) +"|"+ current.toString +"\"];")
          println("  "+ currentParent +" -> "+ id(current))
        case current =>
          val wsBefore = splitWhitespaceBetween(partsHolder getPrevious current)._2
          val wsAfter  = splitWhitespaceBetween(partsHolder getNext current)._1
    
          println("  "+ id(current) +"[label=\""+ escape(wsBefore) +"|"+ current.toString +"|"+ escape(wsAfter) +"\"];")
          println("  "+ currentParent +" -> "+ id(current))
      }
    }
    innerMerge(part)
  }

  
  def main(args : Array[String]) : Unit = {

//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
    val tree = treeFrom("""
      class A(i: Int) {
        val b: String //b-string
        def c: Unit = {
          def d: Int = {
            5
          }
          d
        } //end of c
      }
""")
    
    println(""" 
digraph structs {   
  ordering=out
  //ranksep = 0.2;
  //edge[arrowsize=1, weight=100, arrowhead=none];
  node[shape=record, fontname="Courier", margin=0.05, height=0.4, width=0]
    """)
 
    m(splitIntoParts(tree), essentialParts(tree))
  
    println("\n}")
    
    // why?
    exit(0)
  }
}
