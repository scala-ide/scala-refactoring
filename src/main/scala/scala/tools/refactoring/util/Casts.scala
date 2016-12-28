package scala.tools.refactoring.util

object Casts {
  implicit class CastOps[T](val obj: T) extends AnyVal {
    def tryMatch[U](pf: PartialFunction[T, U]): Option[U] = {
      pf.lift(obj)
    }
  }
}