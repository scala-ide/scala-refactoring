package scala.tools.refactoring

import scala.tools.refactoring.util._
import scala.tools.refactoring.regeneration.FragmentRepository
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.analysis.TreeAnalysis
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.analysis.DeclarationIndexes
import scala.tools.refactoring.regeneration.TreePrinter
import scala.tools.refactoring.regeneration.LayoutHandler
import scala.tools.refactoring.transformation.Transform
import scala.tools.refactoring.regeneration.Merger
import scala.tools.refactoring.regeneration.Partitioner
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.interactive.Global

class Refactoring(val global: Global) extends Transform with Selections with Partitioner with Merger with LayoutHandler with TreePrinter with Tracing with DeclarationIndexes with TreeAnalysis with TreeFactory {
  
  protected val index = new DeclarationIndex
 
  class Selection(file: AbstractFile, from: Int, to: Int) extends TreeSelection(file, from, to)
  
  implicit def abstractFileToTree(file: AbstractFile): global.Tree = global.unitOfFile(file).body
 
  def indexFile(file: AbstractFile): Unit = index processTree file
  
  def refactor(original: global.Tree, changed: global.Tree) = context("main refactoring") {
          
    val partitionedOriginal = splitIntoFragments(original)
    
    trace("Original: %s", partitionedOriginal)
    
    val fr = new FragmentRepository(partitionedOriginal)

    val partitionedModified = essentialFragments(changed, fr)
        
    trace("Modified: %s", partitionedModified)
    
    using(merge(partitionedModified, fr) map (_.print mkString) mkString) { x: String =>
       trace("Result: "+ x)
    }
  }
}
