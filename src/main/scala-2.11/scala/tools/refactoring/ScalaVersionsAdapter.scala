package scala.tools.refactoring

object ScalaVersionAdapters {
  trait CompilerApiAdapters {
    val global: scala.tools.nsc.Global
    import global._

    def annotationInfoTree(info: AnnotationInfo): Tree = info.tree

    def isImplementationArtifact(sym: Symbol): Boolean = {
      sym.isImplementationArtifact
    }
  }
}
