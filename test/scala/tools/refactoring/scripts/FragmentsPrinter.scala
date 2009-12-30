package scala.tools.refactoring.scripts

import scala.tools.refactoring._
import scala.tools.refactoring.util._
import scala.tools.refactoring.tests.util._
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.transformation._

object FragmentsPrinter extends Partitioner with CompilerProvider with Transform with Merger with LayoutHandler with TreePrinter with SilentTracing {
  
  def visualize(tree: global.Tree) {
    
    val partsHolder = new FragmentRepository(splitIntoFragments(tree))
    
    def id(part: Fragment) = part.hashCode.toString
    
    def escape(s: String) = s.replace("\n", "\\n").replace(" ", "·").replace(">", "&gt;").replace("<", "&lt;")
    
    def formatNode(part: Fragment, left: String, middle: String, right: String, color: String = "bisque") = {
      
      val lReq = ""//if(part.requiredBefore.length > 0) "<TD BGCOLOR=\"red\">" + escape(part.requiredBefore mkString "") +"</TD>" else ""
      val rReq = ""//if(part.requiredAfter.length > 0) "<TD BGCOLOR=\"red\">" + escape(part.requiredAfter mkString "") +"</TD>" else ""
      val l = if(left != "") "<TD>" + escape(left) +"</TD>" else ""
      val r = if(right != "") "<TD>" + escape(right) +"</TD>" else ""
    
      id(part) +"[label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"2\"><TR>"+l+lReq+"<TD BGCOLOR=\""+ color +"\">"+ escape(middle) +"</TD>"+r+rReq+"</TR></TABLE>>];"
    }
  
    def innerMerge(part: TreeScope): Unit = {
      
      val currentParent = id(part)
        
      val wsBefore = splitLayoutBetween(partsHolder getPrevious part)._2
      val wsAfter  = splitLayoutBetween(partsHolder getNext part)._1
    
      println(formatNode(part, wsBefore, part.tree.getClass.getSimpleName, wsAfter, "lightgrey"))
      
      part.children foreach {
        case current: TreeScope => 
          innerMerge(current)          
          println("  "+ currentParent +" -> "+ id(current))
        case current: TreeScope#BeginOfScope =>
          val wsAfter  = splitLayoutBetween(partsHolder getNext current)._1
    
          println(formatNode(current, "", "◆", wsAfter))
          println("  "+ currentParent +" -> "+ id(current))
        case current: TreeScope#EndOfScope =>
          val wsBefore = splitLayoutBetween(partsHolder getPrevious current)._2
          
          println(formatNode(current, wsBefore, "◆", ""))
          println("  "+ currentParent +" -> "+ id(current))
        case current =>
          val wsBefore = splitLayoutBetween(partsHolder getPrevious current)._2
          val wsAfter  = splitLayoutBetween(partsHolder getNext current)._1
    
          println(formatNode(current, wsBefore, current.toString, wsAfter))
          println("  "+ currentParent +" -> "+ id(current))
      }
    }
    
    println(""" 
digraph structs {   
  ordering=out
  node[shape=plaintext, fontname="Courier", margin=0.05, height=0.4, width=0]
    """)
    
    innerMerge(essentialFragments(tree, partsHolder))
    //innerMerge(essentialFragments(reverseClassParameters.transform(tree)))
    //innerMerge(essentialFragments(insertValue.transform(tree)))
  
    println("\n}")
  }
  
  def main(args: Array[String]) : Unit = {

//    val tree = treeFrom("""
//trait Person { //
//  def name: String
//  def sex: String 
//}
//""")

    val tree = treeFrom("""
class LR(left: Int, right: String)
""")
    
    visualize(tree)
  
    // why?
    exit(0)
  }
}
