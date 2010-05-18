package scala.tools.refactoring
package sourcegen

import common.{PimpedTrees, Tracing}

trait SourceGen extends PrettyPrinter with ReusingPrinter with PimpedTrees with LayoutHelper with Formatting  with TreeChangesDiscoverer {
  
  this: Tracing =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def createChanges(ts: List[Tree]) = {
    
    val changesByFile = ts groupBy (_.pos.source)
        
    val topLevelTreesByFile = changesByFile map {
      case (source, ts) => (source, findTopLevelTrees(ts))
    }
    
    val changesPerFile = topLevelTreesByFile flatMap {
      case (source, ts) => ts flatMap findAllChangedTrees map {
        case (topLevel, changes) => (source, topLevel, changes)
      }
    }
    
    changesPerFile flatMap {
      case (source, tree, changes) =>
        generate(tree) map {
          case PrintingResult(leading, center, trailing) =>
            common.Change(source.file, tree.pos.start, tree.pos.end, center.asText)
        }
    }
  }
  
  def generate(tree: Tree): Option[PrintingResult] = {
    
    def generateSourceCode(t: Tree, ind: Indentation): Option[PrintingResult] = {
      
      if(t == null || t.isEmpty)
        None
      else if(t.pos == NoPosition)
        Some(prettyPrintTree(generateSourceCode, t, ind))
      else if (t.pos.isRange)
        Some(reuseExistingSource(generateSourceCode, t, ind))
      else 
        None
    }

    val in = new Indentation(defaultIncrement = defaultIndentationStep)
    val initialIndentation = if(tree.pos != NoPosition) indentation(tree) else ""
    
    in.setTo(initialIndentation) {
      generateSourceCode(tree, in)
    }
  }
}