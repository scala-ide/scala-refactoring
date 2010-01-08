package scala.tools.refactoring

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global

class ExtractMethod(override val global: Global, file: AbstractFile, from: Int, to: Int) extends Refactoring(global) {
  
  import global._
  
  def perform(newName: String): String = {
    
    indexFile(file)
    
    val selection = new Selection(file, from, to)
    
    val selectedMethod = selection.enclosingDefDef getOrElse(throw new Exception("no enclosing defdef found"))

    val parameters = inboundLocalDependencies(selection, selectedMethod.symbol)
    
    val returns = outboundLocalDependencies(selection, selectedMethod.symbol)
     
    val newDef = mkDefDef(NoMods, newName, parameters :: Nil, selection.trees ::: (if(returns.isEmpty) Nil else mkReturn(returns) :: Nil))
          
    val call = mkCallDefDef(NoMods, newName, parameters :: Nil, returns)
    
    var newTree = transform(file) {
      case d: DefDef if d == selectedMethod /*ensure that we don't replace from the new method :) */ => {
        if(selection.trees.size > 1) {
          transform(d) {
            case block: Block => {
              mkBlock(replace(block, selection.trees, call :: Nil))
            }
          }
        } else {
          transform(d) {
            case t: Tree if t == selection.trees.head => call
          }
        }
      }
      case tpl @ Template(_, _, body) if body exists (_ == selectedMethod) => {
        tpl.copy(body = replace(body, selectedMethod :: Nil, selectedMethod :: newDef :: Nil))
      }
    }
    
    refactor(file, newTree)
  }
}
