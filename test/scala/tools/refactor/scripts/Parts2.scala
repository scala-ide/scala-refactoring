package scala.tools.refactor.scripts

import scala.tools.refactor.tests.utils._
import scala.tools.refactor.printer._

// cache, optimize, whatever!
class PartsHolder(parts: List[Part]) {
  
  def nextPartToTheRight(part: Part) = {
    val partInOriginal = parts.dropWhile(_ != part)
    
    val wsAfterPart = partInOriginal.tail
    val nextPart = wsAfterPart.dropWhile(_.isWhitespace).head
    val whitespaceBetween = wsAfterPart.takeWhile(_.isWhitespace)
    
    (partInOriginal.head, whitespaceBetween, nextPart)
  }
  
  def nextPartToTheLeft(part: Part) = {
    val partInOriginal = parts.reverse.dropWhile(_ != part)
    
    val wsAfterPart = partInOriginal.tail
    val nextPart = wsAfterPart.dropWhile(_.isWhitespace).head
    val whitespaceBetween = wsAfterPart.takeWhile(_.isWhitespace)
    
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
        (l, r)
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
        StringPart(whitespaceRightOf(parts._1) + whitespaceLeftOf(parts._2)) :: Nil
      }
    }
    
    (modified zip modified.tail) flatMap {
      case p @ (BeginOfFile(_), right) => withWhitespace(p)
      case p @ (left, right) => left :: withWhitespace(p)
    }
  }
  
  def satisfyRequirements(parts: List[Part]): List[Part] = parts match {
    case Nil => Nil
    case (first: WithRequirement) :: second :: rest if first.hasRequirements => 
      val whitespace = second.print
      
      first :: second :: (first.postRequirements.foldRight(List[Part]()) {
        (required: String, ps: List[Part])  =>
          if(whitespace contains required) {
            ps
          } else {
            StringPart(required) :: ps
          }
      }) ::: satisfyRequirements(rest)
    case x :: xs => x :: satisfyRequirements(xs)
  }
}

object Parts2 extends MergeParts {
  
  def main(args : Array[String]) : Unit = {

    import Compiler._
    import compiler._
      
    val tree = treeFrom("class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef")
    //val tree = treeFrom("class A/*aa*/(private val i: Int, s: String, a: Any /*a comment for the Any parameter*/)")
    
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
    
    println(partitionedModified filter (!_.isWhitespace) mkString " → ")
    
    val merged = merge(partitionedOriginal, partitionedModified filter (!_.isWhitespace))
    
    println(merged mkString "|")
    
    val satisfied = satisfyRequirements(merged)
    
    println(satisfied mkString)
    
    // why?
    exit(0)
  }
}
