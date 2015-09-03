/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

/**
 * A class that handles indentation and is passed between
 * the pretty printer and the source generator.
 *
 * defaultIncrement specifies how much the indentation should
 * be incremented for newly generated code (pretty printer).
 */
trait Indentations {

  this: common.Tracing =>

  class Indentation(val defaultIncrement: String, val current: String) {

    def incrementDefault = new Indentation(defaultIncrement, current + defaultIncrement)

    def setTo(i: String) = new Indentation(defaultIncrement, i)

    def needsToBeFixed(oldIndentation: String, surroundingLayout: Layout*) = {
      oldIndentation != current && surroundingLayout.exists(_.contains("\n"))
    }

    def fixIndentation(code: String, oldIndentation: String) = {
      trace("code is %s", code)
      trace("desired indentation is %s", current)
      trace("current indentation is %s", oldIndentation)
      Layout(code.replace("\n"+ oldIndentation, "\n"+ current))
    }
  }

  def indentationString(tree: scala.tools.nsc.Global#Tree): String = {

    def stripCommentFromSourceFile() = {
      if(memoizedSourceWithoutComments contains tree.pos.source.path) {
        memoizedSourceWithoutComments(tree.pos.source.path)
      } else {
        val src = CommentsUtils.stripComment(tree.pos.source.content)
        memoizedSourceWithoutComments += tree.pos.source.path → src
        src
      }
    }

    var i = {
      if(tree.pos.start == tree.pos.source.length || tree.pos.source.content(tree.pos.start) == '\n' || tree.pos.source.content(tree.pos.start) == '\r')
        tree.pos.start - 1
      else
        tree.pos.start
    }
    val contentWithoutComment = stripCommentFromSourceFile()

    while(i >= 0 && contentWithoutComment(i) != '\n') {
      i -= 1
    }

    i += 1

    """\s*""".r.findFirstIn(contentWithoutComment.slice(i, tree.pos.start).mkString).getOrElse("")
  }

  private [this] val memoizedSourceWithoutComments = scala.collection.mutable.Map.empty[String, String]

}