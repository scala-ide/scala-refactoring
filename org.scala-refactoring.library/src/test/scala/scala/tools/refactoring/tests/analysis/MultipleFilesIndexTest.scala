/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import analysis.GlobalIndexes
import tests.util._

import language.{postfixOps, reflectiveCalls}

class MultipleFilesIndexTest extends TestHelper with GlobalIndexes with FreshCompilerForeachTest {

  import global._

  val index = EmptyIndex

  def aggregateFileNamesWithTrees(ts: List[Tree])(conversion: Tree => String) = {
    ts.groupBy(_.pos.source.file.name).toList.sortWith(_._1 < _._1).unzip._2 map { ts =>
      (ts filter (_.pos != NoPosition) map conversion distinct) sortWith(_ < _) mkString ", "
    }
  }

  def buildIndex(pro: FileSet) = {

    val trees = pro.sources map (x => addToCompiler(pro.fileName(x), x)) map (global.unitOfFile(_).body)

    val cuIndexes = global.ask { () =>
      trees map CompilationUnitIndex.apply
    }

    GlobalIndex(cuIndexes)
  }

  def findReferences(pro: FileSet): List[String] = {

    val index = buildIndex(pro)

    val sym = selection(this, pro).selectedSymbols head

    val occurrences = global.ask { () =>
      index.occurences(sym)
    }

    aggregateFileNamesWithTrees(occurrences) { symTree =>
      if(symTree.hasSymbol)
        symTree.symbol.nameString +" on line "+ symTree.pos.line
      else
        symTree.nameString +" on line "+ symTree.pos.line
    }
  }

  def findOverrides(pro: FileSet): List[String] = {

    val index = buildIndex(pro)

    val sym = selection(this, pro).selectedSymbols head

    val overrides = global.ask { () =>
      index.overridesInClasses(sym)
    }

    aggregateFileNamesWithTrees(overrides map index.declaration flatten) { symTree =>
      if(symTree.hasSymbol)
        symTree.symbol.nameString +" on line "+ symTree.pos.line
      else
        symTree.nameString +" on line "+ symTree.pos.line
    }
  }

  def classHierarchy(pro: FileSet): List[String] = {

    val index = buildIndex(pro)

    val sym = selection(this, pro).selectedSymbols head

    aggregateFileNamesWithTrees(index.completeClassHierarchy(sym.owner) map index.declaration flatten) { sym =>
      sym.nameString +" on line "+ sym.pos.line
    }
  }

  def allDeclarations(pro: FileSet): List[String] = {

    val index = buildIndex(pro)

    aggregateFileNamesWithTrees(index.allDeclarations() map (_._2) toList) { sym =>
      if(sym.nameString == "")
        "<no-name> on line "+ sym.pos.line
      else
        sym.nameString +" on line "+ sym.pos.line
    }
  }

  def allSymbols(pro: FileSet): List[String] = {

    val index = buildIndex(pro)

    List((index.allSymbols() map (_.toString) sortWith (_ < _)) mkString (", "))
  }

  @Test
  def findReferencesToClass = new FileSet("p1") {
    """
      package p1
      /*(*/  abstract class  A  /*)*/
      class C extends A
    """ becomes
    """A on line 3, A on line 4"""
    ;
    """
      package p1
      class B extends A {

        val a = new p1.A { }
      }
    """ becomes
    """A on line 3, A on line 5"""
    ;
    """
      package p1.subpackage
      class C extends p1.A {
        val as = new scala.collection.mutable.ListBuffer[p1.A]
        val ass: List[p1.A]
      }

      class X(a: p1.A)
    """ becomes
    """A on line 3, A on line 4, A on line 5, A on line 8"""

  } apply(findReferences)

  @Test
  def findReferencesToMethod = new FileSet("p2") {
    """
      package p2
      class A {
        /*(*/  def method() = "hello world"  /*)*/
      }

      class C {
        (new A).method()
      }
    """ becomes
    """method on line 4, method on line 8"""
    ;
    """
      package p2
      class D(a: A) {
        val am = a.method()
      }
    """ becomes
    """method on line 4"""
    ;
    """
      package p2
      class E extends A {
        val n = method()
        val m = super.method()
        override def method() = "hello 2"
      }

      class F(e: E) {
        val x = e.method()
      }

      class G extends A
    """ becomes
    """method on line 10, method on line 4, method on line 5, method on line 6"""

  } apply(findReferences)

  @Test
  def findReferencesToTraitMethod = new FileSet("p3") {
    """
      package p3
      trait Trait {
        /*(*/  def method: String  /*)*/
      }

      class A extends Trait {
        def method = "impl by def"
      }

      class B extends Trait {
        val method = "impl by val"
      }
    """ becomes
    """method on line 12, method on line 4, method on line 8"""
    ;
    """
      package p3.subpackage
      class C(val method: String) extends p3.Trait
    """ becomes
    """method on line 3"""

  } apply(findReferences)

  @Test
  def findReferencesFromCallSite = new FileSet("p4") {
    """
      package p4
      object O {
        def getInt = 42
      }
    """ becomes
    """getInt on line 4"""
    ;
    """
      package p4
      class A {
        val int = /*(*/  O.getInt  /*)*/
      }
    """ becomes
    """getInt on line 4"""

  } apply(findReferences)

  @Test
  def findValues = new FileSet("p5") {
    """
      package p5
      class A(val s: String) {
        val ss = /*(*/  s  /*)*/
      }
    """ becomes
    """s on line 3, s on line 4"""
    ;
    """
      package p5
      class B(override val s: String)
          extends A(s) {
        val b = new B("b")
        val bb = b.s
      }
    """ becomes
    """s on line 3, s on line 4, s on line 6"""

  } apply(findReferences)

  @Test
  def findSuperCall = new FileSet("p6") {
    """
      package p6
      class A(/*(*/  val s: String  /*)*/, val t: Int)
      class B(override val s: String)
        extends A(s, 5)
    """ becomes
    """s on line 3, s on line 4, s on line 5"""
  } apply(findReferences)

  @Test
  def findCaseClassValues = new FileSet("p7") {
    """
      package p7
      case class A(/*(*/  s: String  /*)*/)
      object B {
        val a = A("hello").s
      }
    """ becomes
    """s on line 3, s on line 5"""
  } apply(findReferences)

  @Test
  def passMethodAsFunction = new FileSet("p8") {
    """
      package p8
      class A {
        /*(*/  def double(i: Int) = i * 2  /*)*/
      }
      object Main {
        val a = new A
        List(1,2,3) map a.double
      }
    """ becomes
    """double on line 4, double on line 8"""
  } apply(findReferences)

  @Test
  def traitImplementation = new FileSet("p9") {
    """
      package p9
      trait Index {
         /*(*/ def countDeclarations(s: String): Int /*)*/
      }
      object Impl {
        val a = new Index { def countDeclarations(s: String) = 0 }
      }
    """ becomes
    """countDeclarations on line 4, countDeclarations on line 7"""
  } apply(findReferences)

  @Test
  def findInImports = new FileSet("p10") {
    """
      package p10
      /*(*/  class A  /*)*/
      class AA extends A
    """ becomes
    """A on line 3, A on line 4"""
    ;
    """
      package unrelated
      import p10.A
      class B extends A
    """ becomes
    """A on line 3, A on line 4"""
  } apply(findReferences)

  @Test
  def inClassHierarchy = new FileSet("p11") {
    """
    trait Abc
    trait /*(*/  B  /*)*/
    """ becomes
    """Abc on line 2, B on line 3"""
    ;
    """
    trait C extends Abc with B
    object Defg extends C
    """ becomes
    """C on line 2, Defg on line 3"""
  } apply(classHierarchy)

  @Test
  def allDeclarationsInClasses = new FileSet {
    """
    trait Abc
    trait B
    """ becomes
    "<no-name> on line 2, Abc on line 2, B on line 3"
    ;
    """
    trait C extends Abc with B
    object Defg extends C
    """ becomes
    "C on line 2, Defg on line 3, this on line 3"
  } apply(allDeclarations)

  @Test
  def allSymbolsInIndex = new FileSet {
    """
    trait Abc
    class B(s: String)
    """ becomes
    "class B, class String, constructor B, constructor Object, object Predef, package <empty>, package scala, trait Abc, type AnyRef, type String, value s, value s"
  } apply(allSymbols)

  @Test
  def allDeclarationsMethods = new FileSet {
    """
    trait Abc {
      def someMethod(): String
    }
    trait B {
      def anotherOne(a: String) = {
        val b = a + ";"
        b
      }
    }
    """ becomes
    "$init$ on line 5, <no-name> on line 2, Abc on line 2, B on line 5, a on line 6, anotherOne on line 6, b on line 7, someMethod on line 3"
    ;
    """
    trait C extends Abc {
      def someMethod(): String = "empty"
      def m = 5
      val a = 42
    }
    """ becomes
    "$init$ on line 2, C on line 2, a on line 5, m on line 4, someMethod on line 3"
  } apply(allDeclarations)

  @Test
  def overriddenMethods = new FileSet {
    """
    trait Abc2 {
      /*(*/def someMethod(): String/*)*/
    }
    trait B2 extends Abc2 {
      val someMethod = "a"
    }
    """ becomes
    "someMethod on line 3, someMethod on line 6"
    ;
    """
    trait C2 extends Abc2 {
      def someMethod(): String = "empty"
    }
    """ becomes
    "someMethod on line 3"
  } apply(findOverrides)

  @Test
  def overriddenMethods2 = new FileSet {
    """
    class Abc {
      /*(*/def someMethod(): String = "a"/*)*/
    }
    trait B extends Abc {
      override val someMethod = "a"
    }
    """ becomes
    "someMethod on line 3, someMethod on line 6"
    ;
    """
    trait C extends Abc {
      override def someMethod(): String = "empty"
    }
    """ becomes
    "someMethod on line 3"
  } apply(findOverrides)

  @Test
  def overriddenMethods3 = new FileSet {
    """
    class Abc {
      def someMethod = "abc"
    }
    class B extends Abc {
      /*(*/override def someMethod = "b"/*)*/
    }
    """ becomes
    "someMethod on line 3, someMethod on line 6"

  } apply(findOverrides)

  @Test
  def overriddenMethods4 = new FileSet {
    """
    class Abc {
      def someMethod = "abc"
    }
    class B extends Abc {
      /*(*/override def someMethod = "b"/*)*/
    }
    class C extends B {
      override def someMethod = "c"
    }
    """ becomes
    "someMethod on line 3, someMethod on line 6, someMethod on line 9"

  } apply(findOverrides)
}

