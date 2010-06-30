/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

trait Requisite {
  self =>
  
  def isRequired(l: Layout, r: Layout): Boolean
  
  def apply(l: Layout, r: Layout): Layout = {
    if(isRequired(l, r)) {
      insertBetween(l, r)
    } else {
      l ++ r
    }
  }
  
  protected def insertBetween(l: Layout, r: Layout) = l ++ getLayout ++ r
  
  def getLayout: Layout
  
  def ++(other: Requisite): Requisite = (self, other) match {
    case (r, NoRequisite) => r
    case (NoRequisite, r) => r
    case _ => new Requisite {
      def isRequired(l: Layout, r: Layout) = self.isRequired(l, r) || other.isRequired(l, r)
      def getLayout = self.getLayout ++ other.getLayout
      override def apply(l: Layout, r: Layout) = {
        val _1 = if(self.isRequired(l, r)) self.getLayout else NoLayout
        val _2 = if(other.isRequired(l, r)) other.getLayout else NoLayout
        l ++ _1 ++ _2 ++ r
      }
    }
  }
}

object Requisite {
    
  def allowSurroundingWhitespace(regex: String) = new Requisite {
    def isRequired(l: Layout, r: Layout) = {
      !l.matches(".*"+ regex +"\\s*$") && !r.matches("^\\s*"+ regex + ".*")
    }
    def getLayout = Layout(regex.replace("\\", ""))
  }
  
  def anywhere(s: String) = new Requisite {
    def isRequired(l: Layout, r: Layout) = {
      !(l.contains(s) || r.contains(s))
    }
    def getLayout = Layout(s)
  }
  
  val Blank = new Requisite {
    def isRequired(l: Layout, r: Layout) = {
      val _1 = l.matches(".*\\s+$")
      val _2 = r.matches("^\\s+.*")
      
      !(_1 || _2)
    }
    val getLayout = Layout(" ")
  }
  
  def newline(indentation: String, force: Boolean = false) = new Requisite {
    def isRequired(l: Layout, r: Layout) = {
      val _1 = l.matches("(?ms).*\n\\s*$")
      val _2 = r.matches("(?ms)^\\s*\n.*")
      !(_1 || _2)
    }
    def getLayout = Layout("\n"+ indentation)
    override def insertBetween(l: Layout, r: Layout) = {
      if(!force && r.asText.startsWith(indentation)) {
        l ++ Layout("\n") ++ r
      } else {
        l ++ getLayout ++ r
      }
    }
  }
}

object NoRequisite extends Requisite {
  def isRequired(l: Layout, r: Layout) = false
  val getLayout = NoLayout
}