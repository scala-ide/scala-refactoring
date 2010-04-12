/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.regeneration

import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.LayoutPreferences
import scala.collection.mutable.ListBuffer

trait LayoutHandler {
  self: Tracing with LayoutPreferences with SourceHelper with Fragments =>
  
  def processRequisites(current: Fragment, layoutAfterCurrent: String, layoutBeforeNext: String, next: Fragment) = context("requisites") {
  
    trace("layout     %s, %s", layoutAfterCurrent, layoutBeforeNext)
    
    // check for overlapping layouts and requirements! => testSortWithJustOne
    def getRequisite(r: Requisite) = if(!(stripComment(layoutAfterCurrent + layoutBeforeNext)).contains(r.check)) {
      trace("Current: %s, next: %s. Layout %s does not contain requisite %s → write %s", current, next, layoutAfterCurrent + layoutBeforeNext, r.check, r.write)
      r.write 
    } else {
      ""
    }
    
    def mapRequirements(rs: Seq[Requisite]) = rs map getRequisite mkString ""

    val NewlineSeparator = """(?ms)(.*?)(\n.*)""".r
    val EndsWithNewline  = """(?ms)(.*?)\n""".r
    val StartWithNewline = """(?ms)\n.*""".r
    
    val(layoutBeforeNewline, layoutAfterNewline) = layoutAfterCurrent match {
      case NewlineSeparator(before, after) => (before, after)
      case s => (s, "")
    }
    
    // layout that is required after a fragment is inserted before a potential newline
    val fixedFirst = layoutBeforeNewline + mapRequirements(current.requiredAfter) + layoutAfterNewline
    
    // when the layout is split at a newline, the newline is part of both layouts. Thus we have to remove two consecutive newlines:
    val finalLayout = ((fixedFirst, layoutBeforeNext) match {
      case(EndsWithNewline(withoutNewline), StartWithNewline()) => withoutNewline
      case _ => fixedFirst
    }) + layoutBeforeNext + mapRequirements(next.requiredBefore)
    
    trace("results in %s", finalLayout)
    
    finalLayout
  }
  
  def fixIndentation(layout: String, existingIndentation: Option[(Int, Int)], isEndOfScope: Boolean, currentScopeIndentation: Int): String = context("fix indentation") {

    if(layout.contains('\n')) {
      
      def indentString(length: Int) = {
        layout.replaceAll("""(?ms)\n[\t ]*""", "\n" + (" " * length))
      }
      
      existingIndentation match {
        case Some((originalScopeIndentation, originalIndentation)) if isEndOfScope && currentScopeIndentation > 0 =>
          indentString(currentScopeIndentation) 
        
        case Some((originalScopeIndentation, originalIndentation)) =>
          trace("this is a reused fragment; at end of scope? %b", isEndOfScope)

            trace("original indentation was %d, original scope indentation was %d", originalIndentation, originalScopeIndentation)
            
            val newIndentation = currentScopeIndentation + (originalIndentation - originalScopeIndentation)
            
            if(newIndentation != originalIndentation || layout.length < newIndentation) {
              trace("new scope's indentation is %d → indent to %d", currentScopeIndentation, newIndentation)
              indentString(newIndentation) 
            }
            else {
              trace("is already correctly indented, layout length is %d", layout.length)
              layout
            }
          
        case None =>
          trace("this is a new fragment")
        
          if(isEndOfScope) {
            trace("at the end of the scope, take scope's parent indentation %d", currentScopeIndentation)
            indentString(currentScopeIndentation)
          } else {
            trace("new scope's indentation is %d → indent to %d ", currentScopeIndentation, currentScopeIndentation + indentationStep)
            indentString(currentScopeIndentation + indentationStep)
          }
      }
    } else layout
  }

  def splitLayoutBetween(parts: Option[(OriginalSourceFragment, OriginalSourceFragment)]) = parts match {
    
    case Some((left, right)) => context("split layout") {

      val (layout, comments) = splitComment(left layout right)

      trace("splitting layout %s between %s and %s. Comments are %s", layout, left, right, comments)
      
      val(l, r, rule) = split(layout)
      
      trace("Rule %s splits layout into %s and %s", rule, l, r)
      
      (mergeLayoutWithComment(l, comments), mergeLayoutWithComment(r reverse, comments reverse) reverse)
    }
    case None => ("", "")
  }
  
  private def mergeLayoutWithComment(l: Seq[Char], c: Seq[Char]) = l zip c map {
    case (' ', _1) => _1
    case (_1, ' ') => _1
    case ('\n', '\n') => '\n'
  } mkString

  private def split(layout: String) = {

    val EmptyParens = """(.*?\(\s*\)\s*)(.*)""".r
    val OpeningBrace = """(.*?\()(.*)""".r
    val Colon = """(.*?)(:.*)""".r
    val Arrow = """(.*?=>\s?)(.*)""".r
    val Equals = """(.*?=\s?)(.*)""".r
    val ClosingBrace = """(?ms)(.*?)(\).*)""".r
    val Comma = """(.*?),\s?(.*)""".r
    val NewLine = """(?ms)(.*?\n)(.*)""".r
    val ImportStatementNewline = """(?ms)(.*)\n(.*?import.*)""".r // imports don't include leading lines, handle in partitioner instead?
    val ImportStatement = """(?ms)(.*)(.*?import.*)""".r
    
    (layout match {
      case Colon(l, r)           => Some(l, r, "Colon")
      case EmptyParens(l, r)     => Some(l, r, "EmptyParens")
      case OpeningBrace(l, r)    => Some(l, r, "OpeningBrace")
      case Arrow(l, r)           => Some(l, r, "Arrow")
      case Equals(l, r)          => Some(l, r, "Equals")
      case ClosingBrace(l, r)    => Some(l, r, "ClosingBrace")
      case ImportStatementNewline(l, r) => Some(l+"\n", "\n"+r, "ImportStatement Newline")
      case _                     => None
    }) orElse (layout match { // Work around https://lampsvn.epfl.ch/trac/scala/ticket/1133
      case ImportStatement(l, r) => Some(l, r, "ImportStatement")
      case NewLine(l, r)         => Some(l, "\n"+r, "NewLine")
      case Comma(l, r)           => Some(l, r, "Comma")
      case s                     => Some(s, "", "NoMatch")
    }) get
  }
}
