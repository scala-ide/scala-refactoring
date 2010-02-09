package scala.tools.refactoring

import scala.tools.refactoring.util.SilentTracing
import scala.tools.refactoring.util.LayoutPreferences
import scala.tools.refactoring.transformation.Transformation
import scala.tools.refactoring.regeneration.Regeneration
import scala.tools.refactoring.analysis.Analysis
import scala.tools.refactoring.util.{Selections, Tracing}
import scala.tools.refactoring.regeneration.FragmentRepository
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global

abstract class Refactoring(val global: Global) extends Analysis with Transformation with Regeneration with Selections with /*Silent*/Tracing with LayoutPreferences {
  
  class Selection(file: AbstractFile, from: Int, to: Int) extends TreeSelection(file, from, to)
    
  type ChangeSet = String
  
  type PreparationResult
  class PreparationError(val cause: String)
  class RefactoringError(val cause: String)
  type RefactoringParameters
  
  implicit def abstractFileToTree(file: AbstractFile): global.Tree = global.unitOfFile(file).body
 
  def indexFile(file: AbstractFile): Unit = index processTree file
  
  def prepare(f: AbstractFile, from: Int, to: Int): Either[PreparationError, PreparationResult]
  
  def perform(prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, ChangeSet]
  
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
