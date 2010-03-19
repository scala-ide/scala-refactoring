package scala.tools.refactoring.common

trait SilentTracing extends Tracing {
  override def print(s: String) = ()
}