package scala.tools.refactoring

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.analysis.Analysis
import scala.tools.refactoring.regeneration.{FragmentRepository, Regeneration}
import scala.tools.refactoring.transformation.Transformation
import scala.tools.refactoring.common.{Selections, Tracing, LayoutPreferences, SilentTracing}

abstract class Refactoring(val global: Global) extends Analysis with Transformation with Regeneration with Selections with /*Silent*/Tracing with LayoutPreferences {
  
  class Selection(file: AbstractFile, from: Int, to: Int) extends TreeSelection(file, from, to)
  
  case class Change(from: Int, to: Int, text: String)
  
  type ChangeSet = List[Change]
  
  type PreparationResult
  
  class PreparationError(val cause: String)
  
  class RefactoringError(val cause: String)
  
  type RefactoringParameters
  
  implicit def abstractFileToTree(file: AbstractFile): global.Tree = global.unitOfFile(file).body
 
  def indexFile(file: AbstractFile): Unit = index processTree file
  
  def prepare(f: AbstractFile, from: Int, to: Int): Either[PreparationError, PreparationResult]
  
  def perform(prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, ChangeSet]
  
  def refactor(original: global.Tree, changed: List[global.Tree], allChanged: List[global.Tree]): ChangeSet = context("main refactoring") {
          
    val partitionedOriginal = splitIntoFragments(original)
    
    trace("Original: %s", partitionedOriginal)
        
    val fr = new FragmentRepository(partitionedOriginal)
    
    changed map { tree =>
      
      val partitionedModified = essentialFragments(tree, fr)
      
      trace("Modified: %s", partitionedModified)
      
      val change = merge(partitionedModified, fr, (allChanged.contains)) map (_.render(fr) mkString) mkString
      
      trace("Result: "+ change)
      
      Change(partitionedModified.start, partitionedModified.end, change)
    }
  }
}
