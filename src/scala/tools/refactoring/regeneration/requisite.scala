package scala.tools.refactoring.regeneration

import scala.collection.mutable.ListBuffer

case class Requisite(check: String, write: String) {
  def this(check: String) = this(check, check)
  override def toString = check
}

trait WithRequisite {
  val requiredAfter = new ListBuffer[Requisite]
  val requiredBefore = new ListBuffer[Requisite]
  def requireAfter(r: Requisite): this.type = {
    requiredAfter += r
    this
  }
  def requireBefore(r: Requisite): this.type = {
    requiredBefore += r
    this
  }
  def hasRequirements = requiredAfter.size > 0 || requiredBefore.size > 0
  def copyRequirements(from: WithRequisite): this.type = {
    from.requiredAfter foreach (requireAfter _)
    from.requiredBefore foreach (requireBefore _)
    this
  }
}
