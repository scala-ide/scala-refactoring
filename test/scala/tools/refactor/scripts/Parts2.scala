package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._
import scala.tools.refactor.transform._

object Parts2 extends Merger with Partitioner with Transform with CompilerProvider {
  
  var partsHolder: PartsHolder = _
  
//  def whitespaceRightOf(part: Part) = splitWhitespaceBetween(partsHolder.nextPartToTheRight(part))._1
//  def whitespaceLeftOf(part: Part)  = splitWhitespaceBetween(partsHolder.nextPartToTheLeft(part))._2
      
  def withWhitespace(current: Part, next: Part): List[Part] = {
    
    println("get ws after part: "+ current)
    
    val (currentFound, wsFound, nextFound) = partsHolder getNext current
    
    if (next == nextFound) {
      println("Whitespace ▒▒"+ (wsFound mkString "") +"▒▒ is between ▒▒"+ current +"▒▒ and ▒▒"+ next +"▒▒.")
      wsFound
    } else {   
      
      StringPart(splitWhitespaceBetween((currentFound, wsFound, nextFound))._1 + splitWhitespaceBetween(partsHolder getPrevious next)._2) :: Nil
      
      //StringPart(whitespaceRightOf(parts._1) + whitespaceLeftOf(parts._2)) :: Nil
      //StringPart("·") :: Nil
    }
  }
  
  def traverseParts(part: CompositePart): List[Part] = {
    
    val list: List[(Part, Part)] = (part.children zip part.children.tail)
    
    println("traversing parts: "+ list)
    
    /*ws begin*/(list flatMap {
      //case (current @ BeginOfFile(_), next) => withWhitespace(current, next)
      case (current: CompositePart, next) => traverseParts(current) ::: withWhitespace(current, next)
      case (current, next) => current :: withWhitespace(current, next)
    }) /*ws end*/
  }

  def main(args : Array[String]) : Unit = {

//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
//      val tree = treeFrom("class A")
    val tree = treeFrom("""class A {
          type C
          val i: Int
        }
    """)

    val partitionedOriginal = splitIntoParts(tree)
    partsHolder = new PartsHolder(partitionedOriginal)
    
    println(partitionedOriginal)
    println("===========")
    
    val newTree = reverseClassParameters.transform(tree)
    val partitionedModified = super.essentialParts(newTree)
    
    println(partitionedModified)
    
    println("===========")
    println(traverseParts(partitionedModified) map (_.print) mkString "")
    
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
