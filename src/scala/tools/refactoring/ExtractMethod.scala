package scala.tools.refactoring

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global

class ExtractMethod(override val global: Global, file: AbstractFile, from: Int, to: Int) extends Refactoring(global) {
  
  import global._
  
  def perform(newName: String): String = {
    
    indexFile(file)
    
    val selection = new Selection(file, from, to)
    
    val trees = selection.trees
        
    val selectedMethod = selection.enclosingDefDef getOrElse(throw new Exception("no enclosing defdef found"))

    val parameters = inboundLocalDependencies(selection, selectedMethod.symbol)
    
    val call = mkCallDefDef(NoMods, newName, parameters :: Nil, outboundLocalDependencies(selection, selectedMethod.symbol))
 
    val returns = mkReturn(outboundLocalDependencies(selection, selectedMethod.symbol))
    
    val newDef  = mkDefDef(NoMods, newName, parameters :: Nil, selection.trees ::: returns :: Nil)
          
    var newTree = transform(file) {
      case tree @ Template(parents, self, body) if body exists (_ == selectedMethod) =>
        new Template(parents, self, replaceTrees(body, selectedMethod :: Nil, selectedMethod :: newDef :: Nil))
    }
    
    newTree = transform(newTree) {
      case d: DefDef if d == selectedMethod =>
        transform(d) {
          case block @ Block(stats, expr) =>
            cleanNoPos {
              Block(replaceTrees(stats, selection.trees, call :: Nil), expr)
            }
        }
    }
    
    refactor(file, newTree)
  }
}
