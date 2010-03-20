package scala.tools.refactoring

import scala.collection.mutable.HashMap
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.analysis.Analysis
import scala.tools.refactoring.regeneration.{FragmentRepository, Regeneration}
import scala.tools.refactoring.transformation.Transformation
import scala.tools.refactoring.common.{Selections, Tracing, LayoutPreferences, SilentTracing}
import scala.tools.refactoring.common.Change

abstract class Refactoring(val global: Global) extends Analysis with Transformation with Regeneration with Selections with SilentTracing with LayoutPreferences {

  def refactor(changed: TreeModifications): List[Change] = context("main refactoring") {
          
    val fragments = new HashMap[AbstractFile, FragmentRepository]
  
    changed.toplevelTrees filter (_.pos.isRange) map { tree =>
    
      val file = tree.pos.source.file
    
      val fr = fragments.getOrElseUpdate(file, 
          new FragmentRepository(splitIntoFragments(global.unitOfFile(file).body) \\ (trace("Original: %s", _))))
      
      val partitionedModified = essentialFragments(tree, fr) \\ (trace("Modified: %s", _))
      
      val change = merge(partitionedModified, fr, { tree =>
        changed.allChangedTrees.contains(tree) || changed.allChangedTrees.exists( t => tree.pos.includes(t.pos))
        }) map (_.render(fr) mkString) mkString // :-/
      
      // we could try to shrink the changes here:
      Change(file, partitionedModified.start, partitionedModified.end, change) \\ (c => trace("Result: "+ c))
    }
  }
}
