

package scala.tools.refactoring.implementations

import org.junit.Test
import org.junit.Assert._

class OrganizeImportsAlgosTest {
  import OrganizeImports.Algos

  @Test
  def testGroupImportsWithTrivialExamples(): Unit = {
    testGroupImports(Nil, Nil, Nil)
    testGroupImports(List("org", "com"), Nil, Nil)
    testGroupImports(Nil, List("com.github.Lausbub"), List(List("com.github.Lausbub")))
  }

  @Test
  def testGroupImportsWithSimpleExamples(): Unit = {
    testGroupImports(
        groups = List("org"),
        imports = List("org.junit.Test", "org.junit.Assert._", "language.postfixOps"),
        expected = List(List("org.junit.Test", "org.junit.Assert._"), List("language.postfixOps")))

    testGroupImports(
        groups = List("java", "scala", "org"),
        imports = List("java.lang.String", "java.lang.Long", "org.junit.Before", "scala.Option.option2Iterable", "language.implicitConversions"),
        expected = List(List("java.lang.String", "java.lang.Long"), List("scala.Option.option2Iterable"), List("org.junit.Before"), List("language.implicitConversions")))
  }

  @Test
  def testGroupImportsWithNastyExamples(): Unit = {
    testGroupImports(
        groups = List("a.c", "a"),
        imports = List("a.b.X", "a.c.Y"),
        expected = List(List("a.c.Y"), List("a.b.X")))

    testGroupImports(
        groups = List("a", "a.c"),
        imports = List("a.b.X", "a.c.Y"),
        expected = List(List("a.b.X"), List("a.c.Y")))

    testGroupImports(
        groups = List("a", "a.b", "ab"),
        imports = List("a.A", "a.b.AB", "ab.Ab1", "ab.Ab2", "abc.Abc1", "abc.Abc2"),
        expected = List(List("a.A"), List("a.b.AB"), List("ab.Ab1", "ab.Ab2"), List("abc.Abc1", "abc.Abc2")))
  }

  private def testGroupImports(groups: List[String], imports: List[String], expected: List[List[String]]): Unit = {
    def getImportExpr(imp: String) = {
      val lastDot = imp.lastIndexOf('.')
      assert(lastDot >= 0)
      imp.substring(0, lastDot)
    }

    val actual = Algos.groupImports(getImportExpr)(groups, imports)
    assertEquals(expected, actual)
  }
}
