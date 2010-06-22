/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import tools.nsc.io.AbstractFile
import common.{PimpedTrees, Tracing, Change}

trait SourceGenerator extends PrettyPrinter with Indentations with ReusingPrinter with PimpedTrees with LayoutHelper with Formatting  with TreeChangesDiscoverer {
  
  self: Tracing =>
  
  import global._
  
  def createFragment(t: Tree): Fragment = {
    generateFragmentsFromTrees(List(t)) map (_._3) head
  }
  
  def createChanges(ts: List[Tree]): List[Change] = context("Create changes") {
    generateFragmentsFromTrees(ts) map {
      case (file, tree, fragment) =>
        Change(file, tree.pos.start, tree.pos.end, fragment.center.asText)
    }
  }
  
  private[refactoring] def generate(tree: Tree, changeset: ChangeSet = AllTreesHaveChanged): Fragment = {

    val initialIndentation = if(tree.hasExistingCode) indentation(tree) else ""
    val in = new Indentation(defaultIndentationStep, initialIndentation)

    print(tree, in, changeset)
  }
  
  private[sourcegen] def generateFragmentsFromTrees(ts: List[Tree]): List[(AbstractFile, Tree, Fragment)] = {
    
    val changesByFile = ts groupBy (_.pos.source)
    
    global.reloadSources(changesByFile.keys filter { source => !global.unitOfFile.get(source.file).isDefined} toList)
    
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
        (source.file, tree, f)
    } toList
  }
  
  override def print(t: Tree, i: Indentation, changeset: ChangeSet): Fragment = {
    
    def hasOriginalTree = findOriginalTree(t).isDefined
    
    def originalTreeNotFound() = {
      if(!global.unitOfFile.get(t.pos.source.file).isDefined)
        throw new common.TreeNotFound(t.pos.source.file.name)  
    }
    
    if(t.hasExistingCode) {
      if(hasOriginalTree)
        super[ReusingPrinter].print(t, i, changeset)
      else {
        originalTreeNotFound()
        EmptyFragment
      }
    }
    else if(t.hasNoCode)
      super[PrettyPrinter].print(t, i, changeset)
    else
      EmptyFragment
  }
}