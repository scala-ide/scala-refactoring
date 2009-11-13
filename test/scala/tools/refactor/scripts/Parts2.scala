package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

object Parts2 extends Merger with Partitioner with Transform with CompilerProvider {
          
  def main(args : Array[String]) : Unit = {

//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
//      val tree = treeFrom("class A")
    val tree = treeFrom("""      
      class A {
        val b: String
        def c: Unit = {
          def d: Int = {
            5
          }
          d
        }
      }""")

    val partitionedOriginal = splitIntoParts(tree)
    
    println(partitionedOriginal.children)
    println("===========")
    
    val newTree = reverseClassParameters.transform(tree)
    val partitionedModified = super.essentialParts(newTree)
    
    println(partitionedModified)
    
    println("===========")
    println(merge(partitionedModified, partitionedOriginal) map (_.print) mkString "")
    
    /*
    
    def filterNonComposites(part: CompositePart): List[Part] = part.children.toList flatMap {
      case eof @ EndOfFile(_) => eof :: Nil
      case p: CompositePart => filterNonComposites(p)
      case part => part :: Nil
    }
    
    val essentialParts = filterNonComposites(partitionedOriginal)
   
    def whitespaceBetween(p: Part, ps: List[Part]): List[Part] = p :: ps match {
      case (eof: EndOfFile) :: Nil => eof :: Nil
      case (p1: OriginalSourcePart) :: (p2: OriginalSourcePart) :: _ => 
        if(p1.end < p2.start) {
          p :: WhitespacePart(p1.end, p2.start, p1.file) :: ps
        } else
          p :: ps
    }
    
    val withWS = essentialParts.foldRight(List[Part]())(whitespaceBetween)
    
    
    println(withWS)
    
    val newTree = reverseClassParameters.transform(tree)
    val partitionedModified = super.essentialParts(newTree)
        
    
    val result = traverseParts(partitionedModified)
    
    println(result mkString "")
    */
//    val partitionedOriginal = splitIntoParts(tree)
//    
//    //println(partitionedOriginal mkString "▒▒")
//    
//    
//    //println(partitionedModified filter (!_.isWhitespace) mkString " → ")
//    
//    val merged = merge(partitionedOriginal, partitionedModified filter (!_.isWhitespace))
//    
//    //println(merged mkString "|")
//    
//    val satisfied = satisfyRequirements(merged)
//    
//    println(satisfied mkString)*/
    
    // why?
    exit(0)
  }
}
