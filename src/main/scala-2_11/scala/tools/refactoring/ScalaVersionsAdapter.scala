package scala.tools.refactoring

object ScalaVersionAdapters {

  trait CompilerApiAdapters { self: common.InteractiveScalaCompiler =>
    import global._

    def annotationInfoTree(info: AnnotationInfo): Tree = info.tree
  }
}
