package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

object PartsPrinter extends Partitioner with CompilerProvider with Transform with Merger with WhitespaceSplitter with TreePrinter {
  
  def visualize(tree: compiler.Tree) {
    
    val partsHolder = new PartsHolder(splitIntoParts(tree))
    
    def id(part: Part) = part.hashCode.toString /*part match {
      case part: CompositePart#EndOfScope => ""+ part.parent.hashCode
      case part: OriginalSourcePart => ""+ part.start.toString + part.toString.hashCode.toString + part.end.toString
      case _ => "?"
    }*/
    
    def escape(s: String) = s.replace("\n", "\\n").replace(" ", "·").replace(">", "&gt;").replace("<", "&lt;")
    
    def formatNode(part: Part, left: String, middle: String, right: String, color: String = "bisque") = {
      
      val lReq = if(part.preRequirements.length > 0) "<TD BGCOLOR=\"red\">" + escape(part.preRequirements mkString "") +"</TD>" else ""
      val rReq = if(part.postRequirements.length > 0) "<TD BGCOLOR=\"red\">" + escape(part.postRequirements mkString "") +"</TD>" else ""
      val l = if(left != "") "<TD>" + escape(left) +"</TD>" else ""
      val r = if(right != "") "<TD>" + escape(right) +"</TD>" else ""
    
      id(part) +"[label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"2\"><TR>"+l+lReq+"<TD BGCOLOR=\""+ color +"\">"+ escape(middle) +"</TD>"+r+rReq+"</TR></TABLE>>];"
    }
  
    def innerMerge(part: CompositePart): Unit = {
      
      val currentParent = id(part)
        
      val wsBefore = splitWhitespaceBetween(partsHolder getPrevious part)._2
      val wsAfter  = splitWhitespaceBetween(partsHolder getNext part)._1
    
      println(formatNode(part, wsBefore, part.tree.getClass.getSimpleName, wsAfter, "lightgrey"))
      
      part.children foreach {
        case current: CompositePart => 
          innerMerge(current)          
          println("  "+ currentParent +" -> "+ id(current))
        case current: CompositePart#BeginOfScope =>
          val wsAfter  = splitWhitespaceBetween(partsHolder getNext current)._1
    
          println(formatNode(current, "", "◆", wsAfter))
          println("  "+ currentParent +" -> "+ id(current))
        case current: CompositePart#EndOfScope =>
          val wsBefore = splitWhitespaceBetween(partsHolder getPrevious current)._2
          
          println(formatNode(current, wsBefore, "◆", ""))
          println("  "+ currentParent +" -> "+ id(current))
        case current =>
          val wsBefore = splitWhitespaceBetween(partsHolder getPrevious current)._2
          val wsAfter  = splitWhitespaceBetween(partsHolder getNext current)._1
    
          println(formatNode(current, wsBefore, current.toString, wsAfter))
          println("  "+ currentParent +" -> "+ id(current))
      }
    }
    
    println(""" 
digraph structs {   
  ordering=out
  node[shape=plaintext, fontname="Courier", margin=0.05, height=0.4, width=0]
    """)
    
    innerMerge(essentialParts(tree, partsHolder))
    //innerMerge(essentialParts(reverseClassParameters.transform(tree)))
    //innerMerge(essentialParts(insertValue.transform(tree)))
  
    println("\n}")
  }
  
  def main(args : Array[String]) : Unit = {

    val tree = treeFrom("""
      class A(i: Int, j: Int) { //
        val b: String
        val c: Int = 5
      }
""")

    
    visualize(tree)
  
    // why?
    exit(0)
  }
}
