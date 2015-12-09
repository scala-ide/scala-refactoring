package scala.tools.refactoring

object ScalaVersionAdapters {
  trait CompilerApiAdapters {
    val global: scala.tools.nsc.Global
    import global._

    def annotationInfoTree(info: AnnotationInfo): Tree = info.original

    def isImplementationArtifact(sym: Symbol): Boolean = {
      sym.isImplementationArtifact || {
        // Unfortunatley, for Scala-2.10, we can not rely on `isImplementationArtifact`
        // as this method might return wrong results. To mitigate this, we fall back to
        // the hack below:
        sym.name.toString.contains("$")
      }
    }
  }
}
