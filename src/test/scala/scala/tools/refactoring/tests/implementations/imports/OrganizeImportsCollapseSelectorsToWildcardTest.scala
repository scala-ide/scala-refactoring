package scala.tools.refactoring.tests.implementations.imports


class OrganizeImportsCollapseSelectorsToWildcardTest extends OrganizeImportsBaseTest {

  def organize(exclude: Set[String] = Set())(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    import refactoring._
    val maxIndividualImports = 2
    val options = List(ExpandImports, SortImports, CollapseImports, CollapseSelectorsToWildcard(maxIndividualImports, exclude), SortImportSelectors)
    val params = new RefactoringParameters(options = options, deps = Dependencies.FullyRecompute)
  }.mkChanges

  @Test
  def collapseImportSelectorsToWildcard() = new FileSet {
    """
      import scala.math.{BigDecimal, BigInt, Numeric}

      object A {
        (BigDecimal, BigInt, Numeric)
      }""" becomes
      """
      import scala.math._

      object A {
        (BigDecimal, BigInt, Numeric)
      }"""
  } applyRefactoring organize()

  @Test
  @Ignore("I don't know why but this test fails when running the complete test suite")
  def dontCollapseImportsWhenRename() = new FileSet {
    val before = """
      import scala.math.{BigDecimal, BigInt, Numeric => N}

      object A {
        (BigDecimal, BigInt, N)
      }"""

    before becomes before
  } applyRefactoring organize()

  @Test
  def dontCollapseWhenCollidingWithExplicitImport() = new FileSet {
    """
      import scala.collection.immutable.{HashSet, BitSet, HashMap}
      import scala.collection.mutable.{ArrayStack, ArrayBuilder, ArrayBuffer}

      object MyObject {
        (BitSet, HashMap, HashSet)
        (ArrayBuffer, ArrayBuilder, ArrayStack)
      }""" becomes
      """
      import scala.collection.immutable._
      import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, ArrayStack}

      object MyObject {
        (BitSet, HashMap, HashSet)
        (ArrayBuffer, ArrayBuilder, ArrayStack)
      }"""
  } applyRefactoring organize()

  @Test
  def dontCollapseWhenPackageInExcludes() = new FileSet {
    val before = """
      import scala.collection.immutable.{BitSet, HashMap, HashSet}

      object MyObject {
        (BitSet, HashMap, HashSet)
      }"""

    before becomes before
  } applyRefactoring organize(Set("scala.collection.immutable"))

}