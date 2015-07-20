package scala.tools.refactoring

package object common {
  /**
   * The selected tracing implementation.
   *
   * Use [[SilentTracing]] for production; consider [[DebugTracing]] for debugging.
   */
  type TracingImpl = SilentTracing
}
