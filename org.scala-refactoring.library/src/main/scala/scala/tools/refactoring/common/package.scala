package scala.tools.refactoring

package object common {
  /**
   * The selected tracing implementation.
   *
   * Use [[SilentTracing]] for production; consider [[ConsoleTracing]] for debugging.
   */
  type TracingImpl = SilentTracing
}
