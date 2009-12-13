package scala.tools.refactoring.regeneration

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.util.SourceFile

object SourceHelper {
  
  def indentationLength(f: Fragment): Option[Int] = f match {
    case f: OriginalSourceFragment => try {
      if(f.isEndOfScope) // end of scope's 'start' points to 'end' and can therefore be on the newline
	      Some(indentationLength(f.start-1, f.file.content))
	    else
	      Some(indentationLength(f.start, f.file.content))
    } catch {
      case _: UnsupportedOperationException => None
      case e => throw e
    }
    case _ => None
  }

  def indentationLength(tree: Trees#Tree): Int = {
    indentationLength(tree.pos.start, tree.pos.source.content)
  }
  
  def indentationLength(start: Int, content: Seq[Char]) = {
    var i = if(start == content.length) start - 1 else start
        
    while(i >= 0 && content(i) != '\n')
      i -= 1
    i += 1
        
    val indentation = """\s*""".r.findFirstIn(content.slice(i, start) mkString).getOrElse("")

    indentation.length
  }
  
  def forwardsTo(to: Char, max: Int)(offset: Int, content: Seq[Char]): Option[Int] = {
    var i = offset
    
    while(i < max && i < content.length - 1 && content(i) != to) {
      i += 1
    }
    
    if(i < content.length && content(i) == to)
      Some(i)
    else
      None
  }
    
  def skipLayoutTo(to: Char)(offset: Int, content: Seq[Char]): Option[Int] = {
    
    var i = offset
    // remove the comment
    
    while(i < content.length - 1 && Character.isWhitespace(content(i))) {
      i += 1
    }
    
    if(i < content.length && content(i) == to)
      Some(i + 1) //the end points to the character _after_ the found character
    else
      None
  }
  
  def backwardsSkipLayoutTo(to: Char)(offset: Int, content: Seq[Char]): Option[Int] = {
    
    if( offset >= 0 && offset < content.length && content(offset) == to) 
      return Some(offset)
      
    var i = offset - 1
    // remove the comment
    
    while(i > 0 && Character.isWhitespace(content(i))) {
      i -= 1
    }
    
    if(i >= 0 && content(i) == to)
      Some(i)
    else
      None
  }
  
  def stripComment(s: String) = splitComment(s)._1
  
  def splitComment(s: String): Pair[String, String] = {
           
    var nestingLevel = 0
    var lineComment = false
    var nextToComment = false
    
    val(comment: Seq[_], text: Seq[_]) = ((s.toList zip (s +" ").toList.tail) map {
      
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
      case _ => throw new Exception("argh!")
    } mkString
  }
}
