package scala.tools.refactoring.sourcegen

object SourceGen3 {
//  
//  def runTerm {
//  
//    sealed abstract class Term
//    case class  TmVar(name: String) extends Term
//    case class  TmAbs(name: String, t: Term) extends Term
//    case class  TmApp(t1: Term, t2: Term) extends Term
//    
//    implicit def termToTransformationFunction(term: Term) = (t: (Term => Term)) => term match {
//      case TmVar(_) =>
//        term
//      case TmAbs(n, t1) => 
//        TmAbs(n, t(t1))
//      case TmApp(t1, t2) => 
//        TmApp(t(t1), t(t2))
//    }
//
//   new Traversal[Term] {
//
//      val isUppercase = filter {
//        case TmVar(name) => Character isUpperCase name(0)
//      }
//      
//      val isTmApp = transform[Term, Term] {
//        case t: TmApp => t 
//      }
//      
//      val isTmAbs = transform[Term, Term] {
//        case t: TmAbs => t 
//      }
//      
//      val rename = transform[Term, Term] {
//        case TmVar(name) => 
//          TmVar(name.toLowerCase)
//      }
//      
//      val term = TmAbs("t", TmAbs("f", TmVar("t")))
//      
//      println(term)
//      
//      val res = bottomup(some(isUppercase andThen rename)) apply term
//      
//      println(res)
//      
//      val x = visit[String](res.get)(not(isUppercase) orElse isTmApp orElse isTmAbs) {(t, term) => term match {
//        case TmVar(name) => 
//          name
//        case TmAbs(n, t1) =>
//          t(t1) map ( "\\"+ n +"."+ _  ) getOrElse "ignored"
//        case TmApp(t1, t2) =>
//           t(t1) flatMap( tt1 => t(t2) map (tt2 => "("+ tt1 +")("+ tt2 +")")) getOrElse "ignored"
//        }
//      }
//      
//      x map println
//    } 
//  
//    def children2(term: Term) = new ((Term => String) => String) {
//        def apply(t: Term => String): String = {
//          term match {
//            case TmVar(name) =>
//              name
//            case TmAbs(n, t1) =>
//              t(t1)
//            case TmApp(t1, t2) => 
//              t(t1) + t(t2)
//          }
//        }
//      }
//      
//    new Transformations[Term, String](children2) {
//  
//  
//        val toS = transform[Term, String] {
//          case TmVar(name) => name//TmApp(TmAbs(name+name, TmVar(name+name)), TmVar(name))
//          case TmAbs(n, t1) => "\\"+ n +"."
//        }
//        
//        val term = TmAbs("x", TmAbs("x",  TmAbs("x", TmVar("x"))))
//        
//        println(term)
//        println((bottomup(toS)) apply term)
//        
//    } 
//  
//  }    
//  
//  def runTree {
//
//    type Tree = String
//    
//    implicit def children(tree: Tree) = new ((Tree => Tree) => Tree) {
//       
//      private def split: List[String] = tree.toList match {
//        case xs => xs map (_.toString)
//      }
//      
//      def apply(t: Tree => Tree): Tree = {
//        
//        val children = split
//        
//        if(children.size > 1) {
//          children map t mkString ""
//        } else {
//          tree
//        }
//      }
//    }    
//    
//    val transformations = new Traversal[Tree]
//    
//    import transformations._    
//    val isUppercase = transform[Tree, Tree] {
//      case x if x.matches("[A-Z]") => x
//    }
//    
//    val blank = transform[Tree, Tree] {
//      case x => "_"
//    }
//    
//    val double = transform[Tree, Tree] {
//      case x => x+x
//    }
//    
//    println((bottomup(isUppercase andThen blank orElse id) andThen double) ("Abcde"))
//    println((topdown (isUppercase andThen blank orElse id) andThen double) ("Abcde"))
//  }
  
//  def runNum {
//
//    val transformations = new Transformations[String, Int] {
//      
//      def toSubject(s: String) = new Subject {
//         
//        private def split: List[String] = s.toList match {
//          case xs => xs map (_.toString)
//        }
//        
//        def apply(t: String => Int) = {
//          
//          val children = split
//          
//          if(children.size > 1) {
//            Integer.parseInt(children map t mkString "") 
//          } else {
//            Integer.parseInt(s)
//          }
//        }
//      }
//    }
//    
//    import transformations._
//    
//    val parseInt = transform[String, Int] {
//      case x if x == "5" => 5
//    }
//    
//    val double = transform[Int, Int] {
//      case x => x*2
//    }
//    
//    val default = transform[String, Int] {
//      case x => 0
//    }
//    
//    
//    //println(all(parseInt andThen double orElse default) ("5f"))
//  }
  
  def main(args: Array[String]) {
   //runTerm
    //runTree
   // runNum
  }
}