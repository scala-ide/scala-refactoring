package scala.tools.refactoring

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.analysis.Analysis
import scala.tools.refactoring.regeneration.{FragmentRepository, Regeneration}
import scala.tools.refactoring.transformation.Transformation
import scala.tools.refactoring.common.{Selections, Tracing, LayoutPreferences, SilentTracing}

abstract class Refactoring(val global: Global) extends Analysis with Transformation with Regeneration with Selections with /*Silent*/Tracing with LayoutPreferences {
 
  type PreparationResult
  
  case class PreparationError(val cause: String)
  
  case class RefactoringError(val cause: String)
  
  type RefactoringParameters
 
  def indexFile(file: AbstractFile): Unit = index processTree file
  
  def prepare(s: Selection): Either[PreparationError, PreparationResult]
  
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, ChangeSet]
  
  def refactor(original: global.Tree, changed: TreeChanges): ChangeSet = context("main refactoring") {
          
    val partitionedOriginal = splitIntoFragments(original) \\ (trace("Original: %s", _))
        
    val fr = new FragmentRepository(partitionedOriginal)
    
    changed.toplevelTrees map { tree =>
      
      val partitionedModified = essentialFragments(tree, fr) \\ (trace("Modified: %s", _))
      
      val change = merge(partitionedModified, fr, (changed.allChangedTrees.contains)) map (_.render(fr) mkString) mkString
      
      Change(partitionedModified.start, partitionedModified.end, change) \\ (c => trace("Result: "+ c))
    }
  }
}
