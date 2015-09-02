package scala.tools.refactoring

import scala.tools.refactoring.common.CompilerApiExtensions

object ScalaVersionAdapters {

  trait CompilerApiAdapters { self: common.InteractiveScalaCompiler =>
    import global._

    def annotationInfoTree(info: AnnotationInfo): Tree = info.original
  }
}
