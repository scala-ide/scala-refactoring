package scala.tools.refactoring.scripts

import scala.tools.nsc.util.NoPosition
import scala.tools.refactoring._
import scala.tools.refactoring.tests.utils._

object TreePrinter extends CompilerProvider with SilentTracing {
  
  import global._
  
  def visualize(tree: Tree) {
        
    def id(t: Tree) = t.hashCode.toString
    
    def escape(s: String) = s.replace("\n", "\\n").replace(" ", "·").replace(">", "&gt;").replace("<", "&lt;")
    
    def formatNode(t: Tree, color: String = "bisque") = {

      id(t) +"[label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"2\"><TR><TD BGCOLOR=\""+ color +"\">"+ escape(t.getClass.getSimpleName) +"</TD></TR></TABLE>>];"
    }
  
    def innerMerge(t: Tree): Unit = {
      
      val current = id(t)
      
      val color = t match {
        case t if t.pos == NoPosition => "lightblue"
        case t if t.pos.isRange => "bisque"
        case _ => "lightgrey"
      }
        
      println(formatNode(t, color))
      
      t match {
        case t: PackageDef => 
          innerMerge(t.pid)
          println("  "+ current +" -> "+ id(t.pid) +"[label=\" identifier\"]")
          t.stats foreach { child => innerMerge(child); println("  "+ current +" -> "+ id(child)+"[label=\" statements\"]")}
        case t: ClassDef =>
          innerMerge(t.impl)
          println("  "+ current +" -> "+ id(t.impl) +"[label=\" implementation\"]")
        case t: Template =>
          t.parents foreach {p => innerMerge(p); println("  "+ current +" -> "+ id(p) +"[label=\" parent\"]")}
          innerMerge(t.self)
          println("  "+ current +" -> "+ id(t.self) +"[label=\" self\"]")
          t.body foreach {p => innerMerge(p); println("  "+ current +" -> "+ id(p) +"[label=\" body\"]")}
        case t: DefDef =>
          if(t.vparamss != Nil && t.vparamss.head != Nil) {
            innerMerge(t.vparamss.head.head)
            println("  "+ current +" -> "+ id(t.vparamss.head.head) +"[label=\" parameter\"]")
          }
          innerMerge(t.tpt)
          println("  "+ current +" -> "+ id(t.tpt) +"[label=\" type\"]")
          innerMerge(t.rhs)
          println("  "+ current +" -> "+ id(t.rhs) +"[label=\" rhs\"]")
        case t: ValDef =>
          innerMerge(t.tpt)
          println("  "+ current +" -> "+ id(t.tpt) +"[label=\" type\"]")
          //innerMerge(t.rhs)
          //println("  "+ current +" -> "+ id(t.rhs) +"[label=\" rhs\"]")
        case t: Select =>
          innerMerge(t.qualifier )
          println("  "+ current +" -> "+ id(t.qualifier) +"[label=\" qualifier\"]")
        case t: Block =>
          innerMerge(t.stats.head)
          println("  "+ current +" -> "+ id(t.stats.head) +"[label=\" statements\"]")
          innerMerge(t.expr)
          println("  "+ current +" -> "+ id(t.expr) +"[label=\" expr\"]")
        case t: Apply =>
          innerMerge(t.fun)
          println("  "+ current +" -> "+ id(t.fun) +"[label=\" function\"]")
        case _ =>
          t.children.foreach { child =>
            innerMerge(child)
            println("  "+ current +" -> "+ id(child))
          }
      }
    }
    
    println(""" 
digraph structs {   
  ordering=out
  node[shape=plaintext, fontname="Courier", margin=0.05, height=0.4, width=0]
edge [fontname="Verdana", fontsize=10, labelfontname="Palatino", labelfontsize=10
       ];
    """)
    
    innerMerge(tree)
  
    println("\n}")
  }
  
  def main(args: Array[String]) : Unit = {
    val tree = treeFrom("""
class Person(val name: String)
""")
    
    visualize(tree)
  
    // why?
    exit(0)
  }
}
