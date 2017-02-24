package scala.tools.refactoring
package tests.implementations.imports

import scala.tools.refactoring.implementations.OrganizeImports
import scala.tools.refactoring.implementations.OrganizeImports.Dependencies
import scala.tools.refactoring.sourcegen.Formatting

class OrganizeImportsEndOfLineTest extends OrganizeImportsBaseTest {
  private def organizeCustomized(
    formatting: Formatting,
    groupPkgs: List[String] = List("java", "scala", "org", "com"),
    useWildcards: Set[String] = Set("scalaz", "scalaz.Scalaz"),
    dependencies: Dependencies.Value = Dependencies.FullyRecompute,
    organizeLocalImports: Boolean = true)(pro: FileSet) = new OrganizeImportsRefatoring(pro, formatting) {
    val oiConfig = OrganizeImports.OrganizeImportsConfig(
      importsStrategy = Some(OrganizeImports.ImportsStrategy.ExpandImports),
      wildcards = useWildcards,
      groups = groupPkgs)
    val params = {
      new refactoring.RefactoringParameters(
        deps = dependencies,
        organizeLocalImports = organizeLocalImports,
        config = Some(oiConfig))
    }
  }.mkChanges

  private def organizeWithUnixEOL(pro: FileSet) = organizeCustomized(formatting = new Formatting { override def lineDelimiter = "\n" })(pro)
  private def organizeWithWindowsEOL(pro: FileSet) = organizeCustomized(formatting = new Formatting { override def lineDelimiter = "\r\n" })(pro)

  @Test
  def shouldPreserveUnixLineSeparator_v1() = new FileSet {
    "package testunix\n\nimport scala.util.Try\nimport java.util.List" becomes
      "package testunix\n\n"
  } applyRefactoring organizeWithUnixEOL

  @Test
  def shouldPreserveUnixLineSeparator_v2() = new FileSet {
    "package testunix\n\nimport scala.util.Try\nimport java.util.List\n" becomes
      "package testunix\n\n\n"
  } applyRefactoring organizeWithUnixEOL

  @Test
  def shouldPreserveUnixLineSeparator_v3() = new FileSet {
    "package testunix\n\nimport scala.util.Try\nimport java.util.List\n\nclass A(val t: Try[Int], val l: List[Int])" becomes
      "package testunix\n\nimport java.util.List\n\nimport scala.util.Try\n\nclass A(val t: Try[Int], val l: List[Int])"
  } applyRefactoring organizeWithUnixEOL

  @Test
  def shouldPreserveUnixLineSeparator_v4() = new FileSet {
    "package testunix\n\nimport scala.util.Try\nimport scala.util.Either\n\nclass A(val t: Try[Int], val l: Either[Int, Int])" becomes
      "package testunix\n\nimport scala.util.Either\nimport scala.util.Try\n\nclass A(val t: Try[Int], val l: Either[Int, Int])"
  } applyRefactoring organizeWithUnixEOL

  @Test
  def shouldPreserveWindowsLineSeparator_v1() = new FileSet {
    "package testwin\r\n\r\nimport scala.util.Try\r\nimport java.util.List" becomes
      "package testwin\r\n\r\n"
  } applyRefactoring organizeWithWindowsEOL

  @Test
  def shouldPreserveWindowsLineSeparator_v2() = new FileSet {
    "package testwin\r\n\r\nimport scala.util.Try\r\nimport java.util.List\r\n" becomes
      "package testwin\r\n\r\n\n"
  } applyRefactoring organizeWithWindowsEOL

  @Test
  def shouldPreserveWindowsLineSeparator_v3() = new FileSet {
    "package testwin\r\n\r\nimport scala.util.Try\r\nimport java.util.List\r\n\r\nclass A(val t: Try[Int], val l: List[Int])" becomes
      "package testwin\r\n\r\nimport java.util.List\r\n\r\nimport scala.util.Try\r\n\r\nclass A(val t: Try[Int], val l: List[Int])"
  } applyRefactoring organizeWithWindowsEOL

  @Test
  def shouldPreserveWindowsLineSeparator_v4() = new FileSet {
    "package testwin\r\n\r\nimport scala.util.Try\r\nimport scala.util.Either\r\n\r\nclass A(val t: Try[Int], val l: Either[Int, Int])" becomes
      "package testwin\r\n\r\nimport scala.util.Either\r\nimport scala.util.Try\r\n\r\nclass A(val t: Try[Int], val l: Either[Int, Int])"
  } applyRefactoring organizeWithWindowsEOL
}
