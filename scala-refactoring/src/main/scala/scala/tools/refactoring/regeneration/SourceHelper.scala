/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package regeneration

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.util.BatchSourceFile

trait CommentHelpers {

  def stripComment(source: SourceFile): String = splitComment(source)._1
  def stripComment(source: String): String = splitComment(source)._1
  
  private val memoizedComments = scala.collection.mutable.Map.empty[String, (String, String)]

  def splitComment(source: String): (String, String) = splitComment(new BatchSourceFile(source, source))
  
  def splitComment(source: SourceFile): (String, String) = (source.content: Seq[Char])/*?*/ match {
    case Nil => ("", "")
    case a :: Nil => (a.toString, " ")
    case _ if memoizedComments contains source.path => memoizedComments(source.path)
    case s =>
      var nestingLevel = 0
      var lineComment = false
      var nextToComment = false
      
      val text = new StringBuilder
      val comment = new StringBuilder
      
      def add(c: Char, t: Char) = {
        comment append c
        text append t
      }
      
      (s ++ " ") reduceLeft({
        case (_1, _2) if nextToComment =>
          nextToComment = false
          add(_1, ' ')
          _2
        
        case ('/', '/') if !lineComment && nestingLevel == 0 => 
          lineComment = true
          nextToComment = true
          add('/', ' ')
          '/'
        
        case ('\n', _2) =>
          lineComment = false
          add('\n', '\n')
          _2
          
        case ('/', '*') if !lineComment =>
          nestingLevel += 1
          nextToComment = true
          add('/', ' ')
          '*'
          
        case ('*', '/') if !lineComment && nestingLevel > 0 =>
          nestingLevel -= 1
          nextToComment = true
          add('*', ' ')
          '/'
          
        case (_1, _2) if lineComment || nestingLevel > 0 => 
          add(_1, ' ')
          _2
          
        case (_1, _2) =>
          add(' ', _1)
          _2
      }: (Char, Char) => Char)
      
      memoizedComments += source.path → (text mkString, comment mkString)
      
      (text mkString, comment mkString)
  }
  
  def liftComment(s: SourceFile)(body: String => String) = {
   
    val(rest, comments) = splitComment(s)

    val res = body(rest mkString)
    
    assert(res.length == rest.length)
    
    (res zip comments) map {
      case (' ', _1 ) => _1
      case (_1 , ' ') => _1
      case ('\n', '\n') => '\n'
      case _ => assert(false)
    } mkString
  }  
}


trait SourceCodeHelpers extends CommentHelpers {
  
  val global: scala.tools.nsc.interactive.Global
  
  def indentationLength(tree: global.Tree): Int = {
    indentationLength(tree.pos.start, tree.pos.source)
  }
  
  def indentation(tree: global.Tree): String = {
    indentation(tree.pos.start, tree.pos.source)
  }
  
  def indentation(start: Int, source: SourceFile): String = {
    var i = if(start == source.length || source.content(start) == '\n') start - 1 else start
    val contentWithoutComment = stripComment(source)
        
    while(i >= 0 && contentWithoutComment(i) != '\n')
      i -= 1
    i += 1
        
    """\s*""".r.findFirstIn(contentWithoutComment.slice(i, start) mkString).getOrElse("")
  }
  
  def indentationLength(start: Int, source: SourceFile) = {

    indentation(start, source).length
  }
  
  def forwardsTo(to: Char, max: Int)(offset: Int, source: SourceFile): Option[Int] = {
    var i = offset
    val contentWithoutComment = stripComment(source)
    
    while(i < max && i < contentWithoutComment.length - 1 && contentWithoutComment(i) != to) {
      i += 1
    }
    
    if(i < contentWithoutComment.length && contentWithoutComment(i) == to)
      Some(i)
    else
      None
  }
    
  def skipLayoutTo(to: Char)(offset: Int, source: SourceFile): Option[Int] = {
    var i = offset
    val contentWithoutComment = stripComment(source)
    
    while(i < contentWithoutComment.length - 1 && contentWithoutComment(i) != to && Character.isWhitespace(contentWithoutComment(i))) {
      i += 1
    }
    
    if(i < contentWithoutComment.length && contentWithoutComment(i) == to)
      Some(i + 1) //the end points to the character _after_ the found character
    else
      None
  }
  
  def backwardsSkipLayoutTo(to: Char)(offset: Int, source: SourceFile): Option[Int] = {
    
    val contentWithoutComment = stripComment(source)
    
    if( offset >= 0 && offset < contentWithoutComment.length && contentWithoutComment(offset) == to) 
      return Some(offset)
      
    var i = offset - 1
    
    while(i > 0 && contentWithoutComment(i) != to && Character.isWhitespace(contentWithoutComment(i))) {
      i -= 1
    }
    
    if(i >= 0 && contentWithoutComment(i) == to)
      Some(i)
    else
      None
  }
}

trait SourceHelper extends SourceCodeHelpers {
    
  self: scala.tools.refactoring.regeneration.Fragments =>
  
  val global: scala.tools.nsc.interactive.Global
  
  def indentationLength(f: Fragment): Option[Int] = f match {
    case f: OriginalSourceFragment => try {
      if(f.isEndOfScope /*&& f.start > 0 */) // end of scope's 'start' points to 'end' and can therefore be on the newline
	      Some(indentationLength(f.start-1, f.file))
	    else
	      Some(indentationLength(f.start, f.file))
    } catch {
      case _: UnsupportedOperationException => None
      case e => throw e
    }
    case _ => None
  }

}
