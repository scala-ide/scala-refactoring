package scala.tools.refactoring.scripts

import scala.tools.refactoring.util.LayoutPreferences
import scala.tools.refactoring.util.Tracing
import scala.tools.refactoring.tests.util._
import scala.tools.refactoring._
import scala.tools.refactoring.regeneration._
import scala.tools.refactoring.transformation._

import scala.tools.nsc.ast._
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

object Parts2 extends CompilerProvider with Regeneration with LayoutPreferences with Tracing {
  
  def main(args : Array[String]) : Unit = {
    
    val src = """
package arith

import scala.util.parsing.combinator._

sealed abstract class Term

case object TmTrue extends Term
case object TmFalse extends Term
case class  TmIf(t1: Term, t2: Term, t3: Term) extends Term
case object TmZero extends Term
case class  TmSucc(t: Term) extends Term
case class  TmPred(t: Term) extends Term
case class  TmIsZero(t: Term) extends Term

class ArithParser extends JavaTokenParsers {
  
  def s: Parser[Term] = 
    t<~";"
  
  def t: Parser[Term] = 
    v |
    "if"~at~"then"~at~"else"~at ^^ { case "if"~tIf~"then"~tThen~"else"~tElse => TmIf(tIf, tThen, tElse) } |
    "pred"~>at ^^ ( TmPred(_) ) | 
    "iszero"~>at ^^ ( TmIsZero(_) ) |
    "succ"~>at ^^ ( TmSucc(_) )
    
  // an argument-term, either a value or a term in parentheses
  def at: Parser[Term] = 
    v | 
    "("~>t<~")"
    
  def v: Parser[Term] = 
    "true"  ^^ ( _ => TmTrue ) | 
    "false" ^^ ( _ => TmFalse ) | 
    nv
    
  def nv: Parser[Term] = 
    "0" ^^ ( _ => TmZero ) | 
    "succ"~>nv ^^ ( TmSucc(_) )
}

object Arith extends ArithParser {
  
  def isNumericVal(t: Term) : Boolean = t match {
    case TmZero => true
    case TmSucc(t1) =>  isNumericVal(t1)
    case _ => false
  }
  
  def isVal(t: Term) : Boolean = t match {
    case TmTrue | TmFalse => true
    case t if isNumericVal(t) => true
    case _ => false
  }

  def eval1(t: Term) : Term = t match {

    case TmIf(TmTrue , t2,  _) => t2
    case TmIf(TmFalse,  _, t3) => t3
    case TmIf(t1       , t2, t3) => TmIf(eval1(t1), t2, t3)
    
    case TmSucc(t1) => TmSucc(eval1(t1))
    
    case TmPred(TmZero) => TmZero
    case TmPred(TmSucc(nv1)) if isNumericVal(nv1) => nv1
    case TmPred(t1) => TmPred(eval1(t1))
    
    case TmIsZero(TmZero) => TmTrue
    case TmIsZero(TmSucc(nv1)) if isNumericVal(nv1) => TmFalse
    case TmIsZero(t1) => TmIsZero(eval1(t1))
    
    case x if isVal(x) => x
    case x => throw new Exception("no match for "+ x)
  }
  
  
  def eval(t: Term) : Term = eval1(t) match {
    case t if isVal(t) => t
    case t => eval(t)
  }
  
  def main(args : Array[String]) {
    
    println(parseAll(s, "if true then false else true;"))
    println(parseAll(s, "if (if true then false else true) then false else true;"))
    println(parseAll(s, "true;"))
    println(parseAll(s, "false;"))
    println(parseAll(s, "pred (succ (succ 0));"))
    println(parseAll(s, "pred (succ (pred 0));"))
    
    println( eval(parseAll(s, "if false then (succ 0) else (succ (succ 0));").get))
  }
}

"""
      
    //val tree = treeFrom(src)
    
    println("ready") 
    
    Console.readLine

    val now = System.currentTimeMillis

    //for(i <- 1 to 10)
    //  splitIntoFragments(tree)
    
    
    
    println("now: "+ (System.currentTimeMillis - now))

    //println(f)
    
    exit(0)
  }
}
