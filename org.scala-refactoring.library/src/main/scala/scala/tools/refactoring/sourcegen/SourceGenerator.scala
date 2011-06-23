/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import common.Tracing
import common.Change
import common.PimpedTrees

trait SourceGenerator extends PrettyPrinter with Indentations with ReusingPrinter with PimpedTrees with LayoutHelper with Formatting with TreeChangesDiscoverer {
  
  self: Tracing with common.CompilerAccess =>
  
  import global._
  
  /**
   * Creates a fragment from a tree, regenerating only those
   * trees that have changed.
   */
  def createFragment(t: Tree): Fragment = {
    generateFragmentsFromTrees(List(t)) map (_._4) head
  }
  
  /**
   * Creates a list of changes from a list of trees, regenerating only those
   * trees that have changed.
   */
  def createChanges(ts: List[Tree]): List[Change] = context("Create changes") {
    generateFragmentsFromTrees(ts) map {
      case (file, tree, range, fragment) =>
        Change(file, range.start, range.end, fragment.center.asText)
    }
  }
  
  /**
   * Creates a string from a tree, regenerating all trees.
   */
  def createText(t: Tree): String = generate(t).asText
  
  object AllTreesHaveChanged extends ChangeSet {
    def hasChanged(t: Tree) = true
  }
  
  private[refactoring] def generate(tree: Tree, changeset: ChangeSet = AllTreesHaveChanged): Fragment = {

    val initialIndentation = if(tree.hasExistingCode) indentation(tree) else ""
    val in = new Indentation(defaultIndentationStep, initialIndentation)

    print(tree, PrintingContext(in, changeset, tree))
  }
  
  private[sourcegen] def generateFragmentsFromTrees(ts: List[Tree]): List[(tools.nsc.io.AbstractFile, Tree, Position, Fragment)] = {
    
    if(ts.exists(_.pos == NoPosition)) {
      throw new IllegalArgumentException("Top-level trees cannot have a NoPosition because we need to get the source file: "+ ts.filter(_.pos == NoPosition).mkString(", "))      
    }
    
    val changesByFile = ts groupBy (_.pos.source)
        
    val topLevelTreesByFile = changesByFile map {
      case (source, ts) => (source, findTopLevelTrees(ts))
    }
    
    val changesPerFile = topLevelTreesByFile flatMap {
      case (source, ts) => ts flatMap findAllChangedTrees map {
        case (topLevel, replaceRange, changes) => (source, replaceRange, topLevel, changes)
      }
    }
    
    if(changesPerFile.isEmpty) {
      trace("No changes were found.")
    }
    
    changesPerFile map {
      case (source, replaceRange, tree, changes) =>
        trace("Creating code for %s. %d tree(s) in changeset.", tree.getClass.getSimpleName, changes.size)
        val f = generate(tree, new ChangeSet {
          def hasChanged(t: Tree) = changes.exists { 
            /*
             * NameTrees have a position that is calculated from their name's length, because their position
             * is not part of the AST. So if we rename a NameTree, the end-position changes whenever the 
             * length of the name changes. To be able to find the original of a renamed tree, we just compare 
             * names, position start and source.
             * 
             * */
            case o: NameTree if o.pos.isRange => t match {
              case t: NameTree if t.pos.isRange => 
                o.nameString == t.nameString && o.pos.source == t.pos.source && o.pos.start == t.pos.start
              case _ => 
                false
            }
            case o => o samePosAndType t
          }
        }) 
        trace("Change: %s", f.center.asText)
        (source.file, tree, replaceRange, f)
    } toList
  }
    
  override def print(t: Tree, ctx: PrintingContext): Fragment = {
    
    def hasOriginalTree = findOriginalTree(t).isDefined
    
    def originalTreeNotFound() = {
      if(!compilationUnitOfFile(t.pos.source.file).isDefined)
        throw new common.TreeNotFound(t.pos.source.file.name)  
    }
    
    if(t.hasExistingCode) {
      if(hasOriginalTree)
        reusingPrinter.dispatchToPrinter(t, ctx)
      else {
        originalTreeNotFound()
        EmptyFragment
      }
    }
    else if(t.hasNoCode)
      prettyPrinter.dispatchToPrinter(t, ctx)
    else
      EmptyFragment
  }
}
