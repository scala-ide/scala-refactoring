package scala.tools

package object refactoring {
  
  /**
   * Safe way to get a simple class name from an object. 
   * 
   * Using getClass.getSimpleName can sometimes lead to InternalError("Malformed class name") 
   * being thrown, so we catch that. Probably related to #SI-2034
   */
  def getSimpleClassName(o: Object): String = try {
    o.getClass.getSimpleName
  } catch {
    case _: InternalError => o.getClass.getName
  }
} 