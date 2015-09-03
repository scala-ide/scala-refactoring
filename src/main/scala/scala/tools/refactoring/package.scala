package scala.tools

import scala.tools.nsc.interactive.PresentationCompilerThread

package object refactoring {

  /**
   * Asserts that the current operation is running on the thread
   * of the presentation compiler (PC). This is necessary because many
   * operations on compiler symbols can trigger further compilation,
   * which needs to be done on the PC thread.
   *
   * To run an operation on the PC thread, use global.ask { .. }
   */
  def assertCurrentThreadIsPresentationCompiler() {
    val msg = "operation should be running on the presentation compiler thread"
    assert(Thread.currentThread.isInstanceOf[PresentationCompilerThread], msg)
  }

  /**
   * Safe way to get a simple class name from an object.
   *
   * Using getClass.getSimpleName can sometimes lead to InternalError("Malformed class name")
   * being thrown, so we catch that. Probably related to #SI-2034
   */
  def getSimpleClassName(o: Object): String = try {
    o.getClass.getSimpleName
  } catch {
    case _: InternalError | _: NoClassDefFoundError => o.getClass.getName
  }
}