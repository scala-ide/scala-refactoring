/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import common.Tracing
import common.Change
import common.PimpedTrees
import scala.tools.refactoring.common.TextChange
import scala.reflect.internal.util.SourceFile

trait SourceGenerator extends PrettyPrinter with Indentations with ReusingPrinter with PimpedTrees with LayoutHelper with Formatting with TreeChangesDiscoverer {

  self: Tracing with common.CompilerAccess =>

  import global._

  /**
   * Creates a fragment from a tree, regenerating only those
   * trees that have changed.
   */
  def createFragment(t: Tree): Fragment = {
    generateFragmentsFromTrees(List(t)).map(_._4).head
  }

  /**
   * Creates a list of TextChanges from a list of trees, regenerating only those
   * trees that have changed.
   */
  def createChanges(ts: List[Tree]): List[TextChange] = context("Create changes") {
    generateFragmentsFromTrees(ts) map {
      case (file, tree, range, fragment) =>

        /*
         * We need to fix the end position because the Scala compiler often doesn't
         * have correct ranges for top-level trees.
         * */

        def replacesCuRoot = {
          compilationUnitOfFile(file) exists (_.body.samePos(tree.pos))
        }

        lazy val trailingSrc = {
          range.source.content.slice(range.end, range.source.length)
        }

        def hasTrailingBraceAndSomething = {
          trailingSrc.contains('}') && trailingSrc.length > 1
        }

        val actualEnd = {
          if(replacesCuRoot && hasTrailingBraceAndSomething) {
            // The RangePosition ends before the } that closes the top-level
            // tree, so we include this additional offset in the source code
            // the change replaces, otherwise we sometimes get stray } after
            // a refactoring.
            val offsetBelongingToCuRoot = trailingSrc.takeWhile(_ != '}').size + 1
            range.end + offsetBelongingToCuRoot
          } else {
            endPositionAtEndOfSourceFile(range)
          }
        }

        TextChange(range.source, range.start, actualEnd, fragment.center.asText)
    }
  }

  /**
   * Creates a string from a tree, regenerating all trees.
   *
   * If the sourceFile parameter is passed, it will be used to figure out
   * what kinds of newline separators we should generate. If None is passed,
   * '\n' is used.
   */
  def createText(t: Tree, sourceFile: Option[SourceFile] = None): String = generate(tree = t, sourceFile = sourceFile).asText

  object AllTreesHaveChanged extends ChangeSet {
    def hasChanged(t: Tree) = true
  }

  private[refactoring] def generate(tree: Tree, changeset: ChangeSet = AllTreesHaveChanged, sourceFile: Option[SourceFile]): Fragment = {

    val initialIndentation = if(tree.hasExistingCode) indentationString(tree) else ""
    val in = new Indentation(defaultIndentationStep, initialIndentation)

    print(tree, PrintingContext(in, changeset, tree, sourceFile))
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
        case (topLevel, replaceRange, changes) =>
          (source, replaceRange, topLevel, changes)
      }
    }

    if(changesPerFile.isEmpty) {
      trace("No changes were found.")
    }

    changesPerFile.map {
      case (source, replaceRange, tree, changes) =>
        trace("Creating code for %s. %d tree(s) in changeset.", getSimpleClassName(tree), changes.size)
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
        }, Some(source))

        trace("Change: %s", f.center.asText)

        val pos = adjustedStartPosForSourceExtraction(tree, replaceRange)

        // In some cases the replacement source starts with a space, and the replacing range
        // also has a space leading up to it. In that case, we drop the leading space from the
        // replacement fragment.
        val replacementWithoutLeadingDuplicateSpace = {
          if(pos.start > 0 && source.content(pos.start - 1) == ' ' && f.center.asText.startsWith(" ")) {
            Fragment(f.center.asText.tail)
          } else {
            f
          }
        }
        (source.file, tree, pos, replacementWithoutLeadingDuplicateSpace)
    }.toList
  }

  override def print(t: Tree, ctx: PrintingContext): Fragment = {
    if(t.hasExistingCode)
      reusingPrinter.dispatchToPrinter(t, ctx)
    else if(t.hasNoCode)
      prettyPrinter.dispatchToPrinter(t, ctx)
    else
      EmptyFragment
  }
}
