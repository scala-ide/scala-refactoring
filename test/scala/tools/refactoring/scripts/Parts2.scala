package scala.tools.refactoring.scripts

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

object Parts2 extends CompilerProvider {
  
  def main(args : Array[String]) : Unit = {
    
    val src = """
class Test {
  def calculate(i: Int): Int = {
    val inc: (Int => Int) = _ + 1
    //aaa
    val b = 1 + inc(1) + get
/*(*/val c = b + 1/*)*/
    c
  }
  
  def get = 5
}
"""
    
    val file = compile(src)
    
    val refactoring = new Refactoring(global)
    
    import refactoring._
    import refactoring.global._
    
    val tree: Tree = file
    
    refactoring indexFile file
    
    val (from, to) = (src.indexOf("/*(*/"), src.indexOf("/*)*/"))
    
    val selection = new Selection(file, from, to)
    
    val trees = selection.trees
        
    val selectedMethod = selection.enclosingDefDef getOrElse(throw new Exception("no enclosing defdef found"))

    val parameters = inboundLocalDependencies(selection, selectedMethod.symbol)
    
    val call = mkCallDefDef(NoMods, "newMethod", parameters :: Nil, outboundLocalDependencies(selection, selectedMethod.symbol))
 
    val returns = mkReturn(outboundLocalDependencies(selection, selectedMethod.symbol))
    
    val newDef  = mkDefDef(NoMods, "newMethod", parameters :: Nil, selection.trees ::: returns :: Nil)
          
    var newTree = transform(file) {
      case tree @ Template(parents, self, body) if body exists (_ == selectedMethod) =>
        new Template(parents, self, newDef :: body)
    }
    
    newTree = transform(newTree) {
      case defdef: DefDef if defdef == selectedMethod =>
        refactoring.transform(defdef) {
          case block @ Block(stats, expr) if block == defdef.rhs =>
            cleanNoPos {
              Block(replaceTrees(stats, selection.trees, call), expr)
            }
        }
    }
    
    val result = refactor(file, newTree)
        
    println(result)
    
    exit(0)
  }
}
