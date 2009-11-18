package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

object PartsPrinter extends Partitioner with CompilerProvider with Transform with Merger with WhitespaceSplitter with TreePrinter {
  
  def visualize(tree: compiler.Tree) {
    
    val partsHolder = new PartsHolder(splitIntoParts(tree))
    
    def id(part: Part) = part match {
      case part: OriginalSourcePart => ""+ part.start +"999"+ part.end
      case _ => "?"
    }
    
    def escape(s: String) = s.replace("\n", "\\n").replace(" ", "·").replace(">", "&gt;")
    
    def formatNode(id: String, left: String, middle: String, right: String, color: String = "bisque") = {
      
      val l = if(left != "") "<TD>" + escape(left) +"</TD>" else ""
      val r = if(right != "") "<TD>" + escape(right) +"</TD>" else ""
    
      id +"[label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"2\"><TR>"+l+"<TD BGCOLOR=\""+ color +"\">"+ middle +"</TD>"+r+"</TR></TABLE>>];"
    }
  
    def innerMerge(part: CompositePart): Unit = {
      
      val currentParent = id(part)
        
      val wsBefore = splitWhitespaceBetween(partsHolder getPrevious part)._2
      val wsAfter  = splitWhitespaceBetween(partsHolder getNext part)._1
    
      println(formatNode(currentParent, wsBefore, /*part.tree.getClass.toString.split("\\$").last*/"<>", wsAfter, "lightgrey"))
      
      part.children foreach {
        case current: CompositePart => 
          innerMerge(current)
        case current: CompositePart#BeginOfScope =>
          val wsAfter  = splitWhitespaceBetween(partsHolder getNext current)._1
    
          println(formatNode(id(current), "", "◆", wsAfter))
        case current: CompositePart#EndOfScope =>
          val wsBefore = splitWhitespaceBetween(partsHolder getPrevious current)._2
    
          println(formatNode(id(current), wsBefore, "◆", ""))
        case current =>
          val wsBefore = splitWhitespaceBetween(partsHolder getPrevious current)._2
          val wsAfter  = splitWhitespaceBetween(partsHolder getNext current)._1
    
          println(formatNode(id(current), wsBefore, current.toString, wsAfter))
      }
            
      part.children foreach (part => println("  "+ currentParent +" -> "+ id(part)))
    }
    
    println(""" 
digraph structs {   
  ordering=out
  node[shape=plaintext, fontname="Courier", margin=0.05, height=0.4, width=0]
    """)
    
    innerMerge(essentialParts(tree))
  
    println("\n}")
  }
  
  def main(args : Array[String]) : Unit = {

    val tree = treeFrom("""
class A {
  def test = {
    5
    5
  }
}
        """)
    
    visualize(tree)
  
    // why?
    exit(0)
  }
}
