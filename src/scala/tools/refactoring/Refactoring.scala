package scala.tools.refactoring

import scala.tools.refactoring.util.LayoutPreferences
import scala.tools.refactoring.transformation.Transformation
import scala.tools.refactoring.regeneration.Regeneration
import scala.tools.refactoring.analysis.Analysis
import scala.tools.refactoring.util.{Selections, Tracing}
import scala.tools.refactoring.regeneration.FragmentRepository
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global

class Refactoring(val global: Global) extends Analysis with Transformation with Regeneration with Selections with Tracing with LayoutPreferences {
  
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
