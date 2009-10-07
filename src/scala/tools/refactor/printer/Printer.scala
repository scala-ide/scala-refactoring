package scala.tools.refactor.printer

import scala.tools.nsc.util._
import scala.tools.nsc.ast._

object Printer {
  
  def apply(out: Appendable, trees: Trees, root: Trees#Tree) : Unit = {
    
      import trees._
      
      type Tree = Trees#Tree
      
      def print(tree: Tree) = tree match {
        
        case PackageDef(pid, stats) => 
        
        
        
        case _ => out append "unhandled"
        
      }
      
      print(root)
  }

}
