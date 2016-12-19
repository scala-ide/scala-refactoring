package scala.tools.refactoring.tests.implementations.imports

import scala.tools.refactoring.implementations.OrganizeImports
import scala.tools.refactoring.implementations.OrganizeImports.Dependencies

class OrganizeImportsScalaSpecificTests extends OrganizeImportsBaseTest {
  private def organizeCustomized(
    groupPkgs: List[String] = List("java", "scala", "org", "com"),
    useWildcards: Set[String] = Set("scalaz", "scalaz.Scalaz"),
    dependencies: Dependencies.Value,
    organizeLocalImports: Boolean = true)(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val oiConfig = OrganizeImports.OrganizeImportsConfig(
      importsStrategy = Some(OrganizeImports.ImportsStrategy.ExpandImports),
      wildcards = useWildcards,
      groups = groupPkgs)
    val params = {
      new refactoring.RefactoringParameters(
        options =
            refactoring.PrependScalaPackage ::
            Nil,
        deps = dependencies,
        organizeLocalImports = organizeLocalImports,
        config = Some(oiConfig))
    }
  }.mkChanges

  @Test
  def shouldNotRemoveImportWhenExtendedClassHasInferredTypeParam() = new FileSet {
    """
    package acme

    class Extended[T](val a: T, t: String)
    """ isNotModified

    """
    /*<-*/
    package acme.test

    import acme.Extended

    class Tested(val id: Int) extends Extended(id, "text")
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenJustPackage_v3() = new FileSet {
    """
    /*<-*/
    package tested

    import scala.reflect.macros.whitebox

    class Tested(val c: whitebox.Context)
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)
}
