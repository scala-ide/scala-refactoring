package scala.tools.refactoring.sourcegen

class Transformations[C] {
  
  abstract class Transformation[X, Y] extends Function[X, Option[Y]] {
    
    self =>
    
    def apply(in: X): Option[Y]

    def andThen[Z](t: => Transformation[Y, Z]) = new Transformation[X, Z] {
      def apply(in: X): Option[Z] = {
        self(in) flatMap t
      }
    }

    def orElse(t: => Transformation[X, Y]) = new Transformation[X, Y] {
      def apply(in: X): Option[Y] = {
        self(in) orElse t(in)
      }
    }
  }

  def transform[X, Y](f: => PartialFunction[X, Y]) = new Transformation[X, Y] {
    def apply(t: X): Option[Y] = f.lift(t)
  }
  
  def filter(f: => PartialFunction[C, Boolean]) = new Transformation[C, C] {
    def apply(t: C): Option[C] = if(f.isDefinedAt(t) && f(t)) Some(t) else None
  }

  def success = transform[C, C] {
    case x => x
  }
  
  def id = success
  
  def fail = new Transformation[C, C] {
    def apply(in: C): Option[C] = None
  }
  
  def not(t: => Transformation[C, C]) = t andThen fail orElse success 
  
  def recursively[X](filter: => Transformation[C, C])(function: => (Transformation[C, X], C) => X) = new Transformation[C, X] {
    def apply(in: C) = {
      
      def recurse: Transformation[C, X] = filter andThen transform[C, X] {
        case c => function(recurse, c)
      }
      recurse(in)
    }
  }
}

class TreeTransformations[C <% (C => C) => C] extends Transformations[C] {
  
  def all(t: => Transformation[C, C]) = new Transformation[C, C] {
    def apply(in: C): Option[C] = {
      Some(in apply { child =>
       t(child) getOrElse (return None)
      })
    }
  }
    
  def topdown(t: => Transformation[C, C]): Transformation[C, C] = t andThen all(topdown (t))
  
  def bottomup(t: => Transformation[C, C]): Transformation[C, C] = all(bottomup (t)) andThen t
  
  def some(t: Transformation[C, C]) = all(t orElse id)
}
