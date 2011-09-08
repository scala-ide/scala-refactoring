/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import tools.nsc.util.SourceFile
import tools.nsc.util.BatchSourceFile

trait CommentHelpers {

  def stripCommentFromSourceFile(source: SourceFile): String = splitComment(source)._1
  def stripComment(source: String): String = splitComment(source)._1
  
  private[this] val memoizedComments = scala.collection.mutable.Map.empty[String, (String, String)]

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
        
        case ('\r', _2) =>
          lineComment = false
          add('\r', '\r')
          _2
          
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
  
  val global: scala.tools.nsc.Global
  
  def indentation(tree: global.Tree): String = {
    indentation(tree.pos.start, tree.pos.source)
  }
  
  def indentation(start: Int, source: SourceFile): String = {
    var i = if(start == source.length || source.content(start) == '\n' || source.content(start) == '\r') start - 1 else start
    val contentWithoutComment = stripCommentFromSourceFile(source)
        
    while(i >= 0 && contentWithoutComment(i) != '\n')
      i -= 1
    i += 1
        
    """\s*""".r.findFirstIn(contentWithoutComment.slice(i, start) mkString).getOrElse("")
  }
}
