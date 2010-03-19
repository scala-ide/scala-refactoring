package scala.tools.refactoring

import scala.collection.mutable.HashMap
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.analysis.Analysis
import scala.tools.refactoring.regeneration.{FragmentRepository, Regeneration}
import scala.tools.refactoring.transformation.Transformation
import scala.tools.refactoring.common.{Selections, Tracing, LayoutPreferences, SilentTracing}
import scala.tools.refactoring.common.Change

abstract class Refactoring(val global: Global) extends Analysis with Transformation with Regeneration with Selections with /*Silent*/Tracing with LayoutPreferences {
 
  type PreparationResult
  
  case class PreparationError(val cause: String)
  
  case class RefactoringError(val cause: String)
  
  type RefactoringParameters
 
  def prepare(s: Selection): Either[PreparationError, PreparationResult]
  
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]]
  
  def refactor(changed: TreeChanges): List[Change] = context("main refactoring") {
          
    val fragments = new HashMap[AbstractFile, FragmentRepository]
  
    changed.toplevelTrees map { tree =>
    
      val file = tree.pos.source.file
    
      val fr = fragments.getOrElseUpdate(file, 
          new FragmentRepository(splitIntoFragments(global.unitOfFile(file).body) \\ (trace("Original: %s", _))))
      
      val partitionedModified = essentialFragments(tree, fr) \\ (trace("Modified: %s", _))
      
      val change = merge(partitionedModified, fr, (changed.allChangedTrees.contains)) map (_.render(fr) mkString) mkString
      
      Change(file, partitionedModified.start, partitionedModified.end, change) \\ (c => trace("Result: "+ c))
    }
  }
}
