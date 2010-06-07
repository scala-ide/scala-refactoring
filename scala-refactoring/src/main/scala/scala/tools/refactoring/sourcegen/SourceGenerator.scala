package scala.tools.refactoring
package sourcegen

import common.{PimpedTrees, Tracing, Change}

trait SourceGenerator extends PrettyPrinter with Indentations with ReusingPrinter with PimpedTrees with LayoutHelper with Formatting  with TreeChangesDiscoverer {
  
  self: Tracing =>
  
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
        trace("Creating code for %s. %d tree(s) in changeset.", tree.getClass.getSimpleName, changes.size)
        val f = generate(tree, new ChangeSet {
          def hasChanged(t: Tree) = changes.contains(t)
        }) 
        trace("Change: %s", f.center.asText)
        Change(source.file, tree.pos.start, tree.pos.end, f.center.asText)
    }
  }
  
  override def print(t: Tree, i: Indentation, changeset: ChangeSet): Fragment = {
    if(t.hasExistingCode) {
      if(findOriginalTree(t).isDefined)
        super[ReusingPrinter].print(t, i, changeset)
      else {
        val root = global.unitOfFile.get(t.pos.source.file)
        val again = findOriginalTree(t)
        
        if(!global.unitOfFile.get(t.pos.source.file).isDefined)
          throw new common.TreeNotFound(t.pos.source.file.name)
        EmptyFragment
        //Predef.error("tree has existing code but original was not found: "+ t)
      }
    }
    else if(t.hasNoCode)
      super[PrettyPrinter].print(t, i, changeset)
    else
      EmptyFragment
  }
  
  private[refactoring] def generate(tree: Tree, changeset: ChangeSet): Fragment = {

    val initialIndentation = if(tree.hasExistingCode) indentation(tree) else ""
    val in = new Indentation(defaultIndentationStep, initialIndentation)

    print(tree, in, changeset)
  }
}