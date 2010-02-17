package scala.tools.refactoring.regeneration

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.SourceFile

trait SourceHelper {
  
  self: scala.tools.refactoring.regeneration.Fragments =>
  
  val global: scala.tools.nsc.Global
  
  def indentationLength(f: Fragment): Option[Int] = f match {
    case f: OriginalSourceFragment => try {
      if(f.isEndOfScope /*&& f.start > 0 */) // end of scope's 'start' points to 'end' and can therefore be on the newline
	      Some(indentationLength(f.start-1, f.file.content))
	    else
	      Some(indentationLength(f.start, f.file.content))
    } catch {
      case _: UnsupportedOperationException => None
      case e => throw e
    }
    case _ => None
  }

  def indentationLength(tree: global.Tree): Int = {
    indentationLength(tree.pos.start, tree.pos.source.content)
  }
  
  def indentationLength(start: Int, content: Seq[Char]) = {
    var i = if(start == content.length || content(start) == '\n') start - 1 else start
    val contentWithoutComment = stripComment(content)
        
    while(i >= 0 && contentWithoutComment(i) != '\n')
      i -= 1
    i += 1
        
    val indentation = """\s*""".r.findFirstIn(contentWithoutComment.slice(i, start) mkString).getOrElse("")

    indentation.length
  }
  
  def forwardsTo(to: Char, max: Int)(offset: Int, content: Seq[Char]): Option[Int] = {
    var i = offset
    val contentWithoutComment = stripComment(content)
    
    while(i < max && i < contentWithoutComment.length - 1 && contentWithoutComment(i) != to) {
      i += 1
    }
    
    if(i < contentWithoutComment.length && contentWithoutComment(i) == to)
      Some(i)
    else
      None
  }
    
  def skipLayoutTo(to: Char)(offset: Int, content: Seq[Char]): Option[Int] = {
    var i = offset
    val contentWithoutComment = stripComment(content)
    
    while(i < contentWithoutComment.length - 1 && contentWithoutComment(i) != to && Character.isWhitespace(contentWithoutComment(i))) {
      i += 1
    }
    
    if(i < contentWithoutComment.length && contentWithoutComment(i) == to)
      Some(i + 1) //the end points to the character _after_ the found character
    else
      None
  }
  
  def backwardsSkipLayoutTo(to: Char)(offset: Int, content: Seq[Char]): Option[Int] = {
    
    val contentWithoutComment = stripComment(content)
    
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
  
  def stripComment(s: Seq[Char]) = splitComment(s)._1
  
  private val memoizedComments = scala.collection.mutable.Map.empty[Seq[Char], (String, String)]
  
  def splitComment(s: Seq[Char]): (String, String) = s match {
    case Nil => ("", "")
    case a :: Nil => (a.toString, " ")
    case s if memoizedComments contains s => memoizedComments(s)
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
      
      memoizedComments += s → (text mkString, comment mkString)
      
      (text mkString, comment mkString)
  }
  
  def liftComment(s: String)(body: String => String) = {
   
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
