package scala.tools.refactoring
package sourcegen

import common.{PimpedTrees, Tracing, Change}

trait SourceGen extends PrettyPrinter with Indentations with ReusingPrinter with PimpedTrees with LayoutHelper with Formatting  with TreeChangesDiscoverer {
  
  this: Tracing =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def createChanges(ts: List[Tree]): Iterable[Change] = context("Create changes") {
    
    val changesByFile = ts groupBy (_.pos.source)
        
    val topLevelTreesByFile = changesByFile map {
      case (source, ts) => (source, findTopLevelTrees(ts))
    }
    
    val changesPerFile = topLevelTreesByFile flatMap {
      case (source, ts) => ts flatMap findAllChangedTrees map {
        case (topLevel, changes) => (source, topLevel, changes)
      }
    }
    
    changesPerFile map {
      case (source, tree, changes) =>
        val f = generate(tree) 
        trace("Change: %s", f.center.asText)
        Change(source.file, tree.pos.start, tree.pos.end, f.center.asText)
    }
  }
  
  override def print(t: Tree, i: Indentation): Fragment = {
    if(t.hasExistingCode)
      super[ReusingPrinter].print(t, i)
    else if(t.hasNoCode)
      super[PrettyPrinter].print(t, i)
    else
      EmptyFragment
  }
  
  private[refactoring] def generate(tree: Tree): Fragment = {

    val initialIndentation = if(tree.hasExistingCode) indentation(tree) else ""
    val in = new Indentation(defaultIndentationStep, initialIndentation)
    
    print(tree, in)
  }
}