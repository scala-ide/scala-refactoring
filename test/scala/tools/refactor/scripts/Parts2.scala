package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._

// cache, optimize, whatever!
class PartsHolder(parts: List[Part]) {
  
  def nextPartToTheRight(part: Part) = {
    val partInOriginal = parts.dropWhile(_ != part)
    
    val wsAfterPart = partInOriginal.tail
    val nextPart = wsAfterPart.dropWhile(_.isWhiteSpace).head
    val whitespaceBetween = wsAfterPart.takeWhile(_.isWhiteSpace)
    
    (partInOriginal.head, whitespaceBetween, nextPart)
  }
  
  def nextPartToTheLeft(part: Part) = {
    val partInOriginal = parts.reverse.dropWhile(_ != part)
    
    val wsAfterPart = partInOriginal.tail
    val nextPart = wsAfterPart.dropWhile(_.isWhiteSpace).head
    val whitespaceBetween = wsAfterPart.takeWhile(_.isWhiteSpace)
    
    (nextPart, whitespaceBetween.reverse, partInOriginal.head)
  }
}

trait MergeParts {
  
  def splitWhitespaceBetween(parts: Triple[Part,List[Part],Part]) = {
          
    val OpeningBrace = """(.*?\()(.*)""".r
    val ClosingBrace = """(.*?)(\).*)""".r
    val Comma = """(.*?),\s*(.*)""".r
    // strip comments!
    
    explain("Splitting whitespace between "+ parts._1 +" and "+ parts._3)
    
    val whitespace = parts._2 mkString ""
    
    ((parts._1, whitespace, parts._3) match {
      case(_, OpeningBrace(l, r), _) => (l, r)
      case(_, ClosingBrace(l, r), _) => (l, r)
      case(_, Comma(l, r),        _) => (l, r)
    }) match {
      case(l, r) => 
        explain("Whitespace ▒▒"+ whitespace +"▒▒ partitions into ▒▒"+ l +"▒▒ and ▒▒"+ r +"▒▒.")
        (StringPart(l), StringPart(r))
    }
  }
  
  def explain(what: String) = println(what)

  def merge(original: List[Part], modified: List[Part]) = {
    
    val partsHolder = new PartsHolder(original)

    def whitespaceRightOf(part: Part) = splitWhitespaceBetween(partsHolder.nextPartToTheRight(part))._1
    def whitespaceLeftOf(part: Part)  = splitWhitespaceBetween(partsHolder.nextPartToTheLeft(part))._2
    
    def withWhitespace(parts: Pair[Part, Part]): List[Part] = {
      
      val(left, ws, right) = partsHolder.nextPartToTheRight(parts._1)
            
      if(right == parts._2) {
        explain("Whitespace ▒▒"+ (ws mkString "") +"▒▒ is between ▒▒"+ left +"▒▒ and ▒▒"+ right +"▒▒.")
        ws
      } else {
        whitespaceRightOf(parts._1) :: whitespaceLeftOf(parts._2) :: Nil
      }
    }
    
    (modified zip modified.tail) flatMap {
      case p @ (BeginOfFile, right) => withWhitespace(p)
      case p @ (left, right) => left :: withWhitespace(p)
    }
  }
}

object Parts2 extends MergeParts {
  
  def main(args : Array[String]) : Unit = {

    import Compiler._
    import compiler._
      
//    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
    val tree = treeFrom("class A/*aa*/(private val i: Int, s: String)")
    
    val transformer = new Transformer {
      override def transform(tree: Tree): Tree = super.transform(tree) match {
        case Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
        case x => x
      }
    }
    
    val newTree = transformer.transform(tree)
    
    val partitionedOriginal = Partitioner(compiler, tree)
    
    println(partitionedOriginal mkString "▒▒")
    
    val partitionedModified = Partitioner(compiler, newTree)
    
    println(partitionedModified filter (!_.isWhiteSpace) mkString " → ")
    
    val merged = merge(partitionedOriginal, partitionedModified filter (!_.isWhiteSpace))
    
    println(merged mkString "")
    
    // why?
    exit(0)
  }
}
