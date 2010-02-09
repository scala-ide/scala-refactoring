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
  
  def splitComment(s: Seq[Char]): Pair[String, String] = {
           
    var nestingLevel = 0
    var lineComment = false
    var nextToComment = false
    
    val(comment: Seq[_], text: Seq[_]) = ((s.toList zip (s ++ " ").toList.tail) map {
      
      case (_1, _) if nextToComment =>
        nextToComment = false
        (_1, ' ')
      
      case ('/', '/') if !lineComment && nestingLevel == 0 => 
        lineComment = true
        nextToComment = true
        ('/', ' ')
      
      case ('\n', _ ) =>
        lineComment = false
        ('\n', '\n')
        
      case ('/', '*') if !lineComment =>
        nestingLevel += 1
        nextToComment = true
        ('/', ' ')
        
      case ('*', '/') if !lineComment && nestingLevel > 0 =>
        nestingLevel -= 1
        nextToComment = true
        ('*', ' ')
        
      case (_1 , _  ) if lineComment || nestingLevel > 0 => 
        (_1, ' ')
        
      case (_1 , _  ) =>
        (' ', _1)
        
    }).foldRight(List[Char](), List[Char]()) {
      (c, l) => (c._1 :: l._1, c._2 :: l._2)
    }
    
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
