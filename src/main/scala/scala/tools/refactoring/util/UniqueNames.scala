package scala.tools.refactoring.util

import java.util.UUID

object UniqueNames {
  def scalaFile(): String = {
    s"${basename()}.scala"
  }

  def srcDir(): String = {
    s"src-${uid()}"
  }

  def basename(): String = {
    uid()
  }

  def scalaPackage(): String = {
    uid()
  }

  private def uid(): String = {
    def longToName(l: Long) = {
      java.lang.Long.toString(l, Character.MAX_RADIX).replace("-", "_")
    }

    val rid = UUID.randomUUID()
    "uid" + longToName(rid.getLeastSignificantBits) + longToName(rid.getMostSignificantBits)
  }
}
