package scala.tools.refactoring.sourcegen

trait Formatting {
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  val defaultIndentationStep = "  "
  
  // hold formatting preferences
}