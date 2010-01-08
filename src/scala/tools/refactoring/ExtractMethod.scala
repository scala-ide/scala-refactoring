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
          
    var newTree = transform(file) {
      case tree @ Template(parents, self, body) if body exists (_ == selectedMethod) =>
        new Template(parents, self, replaceTrees(body, selectedMethod :: Nil, selectedMethod :: newDef :: Nil)).copyAttrs(tree)
    }
    
    val call = mkCallDefDef(NoMods, newName, parameters :: Nil, returns)
    
    newTree = transform(newTree) {
      case d: DefDef if d == selectedMethod /*ensure that we don't replace from the new method :) */ => {
        if(selection.trees.size > 1) {
          transform(d) {
            case b @ Block(stats, expr) => {
              mkBlock(replaceTrees(stats ::: expr :: Nil, selection.trees, call :: Nil)) copyAttrs b
            }
          }
        } else {
          transform(d) {
            case t: Tree if t == selection.trees.head => call setPos t.pos // replaceTree
          }
        }
      }
    }
    
    refactor(file, newTree)
  }
}
