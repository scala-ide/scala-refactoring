package scala.tools.refactoring.tests.analysis

import scala.tools.refactoring.analysis.ScopeAnalysis
import scala.tools.refactoring.tests.util.TestHelper

import org.junit.Assert._

class ScopeAnalysisTest extends TestHelper with ScopeAnalysis {
  import global._

  @Test
  def simpleScopes() = global.ask { () =>
    val s = toSelection("""
      package demo
      object Demo{
        def fn = {
          val a = 1
          var b = 2
          b = b * a
          /*(*/b/*)*/
        }
      }
      """)

    assertEquals(
      "LocalScope(b) -> LocalScope(a) -> MemberScope(Demo) -> MemberScope(demo)",
      ScopeTree.build(s.root, s.selectedTopLevelTrees.head).toString())
  }

  @Test
  def nestedLocalScopes() = global.ask { () =>
    val s = toSelection("""
      object Demo{
        def fn = {
          val a: Int = {
            if(true){
              val b = 1
              /*(*/b + a/*)*/
            } else {
              2
            }
          }
        }
      }
      """)

    assertEquals(
      "LocalScope(b) -> LocalScope(a) -> MemberScope(Demo) -> MemberScope(<empty>)",
      ScopeTree.build(s.root, s.selectedTopLevelTrees.head).toString())
  }

  @Test
  def LocalScopes() = global.ask { () =>
    val s = toSelection("""
      class Demo(cp: Int){
        def fn(a: Int, b: Int) = {
          /*(*/a * b/*)*/
        }
      }
      """)

    assertEquals(
      "LocalScope(a, b) -> MemberScope(Demo) -> MemberScope(<empty>)",
      ScopeTree.build(s.root, s.selectedTopLevelTrees.head).toString())
  }

  @Test
  def scopesInForEnumerators() = global.ask { () =>
    val s = toSelection("""
      object Demo{
        for(i <- 1 to 10; j <- 1 to 10){
          /*(*/println(i * j)/*)*/
        }
      }
      """)

    assertEquals(
      "LocalScope(j) -> LocalScope(i) -> MemberScope(Demo) -> MemberScope(<empty>)",
      ScopeTree.build(s.root, s.selectedTopLevelTrees.head).toString())
  }

  @Test
  def scopeFromCase() = global.ask { () =>
    val s = toSelection("""
      object Demo{
        (1, 2) match {
          case (x: Int, y: Int) => println(/*(*/x*y/*)*/)
        }
      }
      """)

    assertEquals(
      "LocalScope(x, y) -> MemberScope(Demo) -> MemberScope(<empty>)",
      ScopeTree.build(s.root, s.selectedTopLevelTrees.head).toString())
  }

  @Test
  def nestedClassScopes() = global.ask { () =>
    val s = toSelection("""
      object Demo {
        def fn(p: Int) = {
          class A {
            val a = /*(*/p/*)*/
          }

          new A.a
        }
      }
      """)

    assertEquals(
      "MemberScope(A) -> LocalScope(p) -> MemberScope(Demo) -> MemberScope(<empty>)",
      ScopeTree.build(s).toString())
  }

  @Test
  def visibility() = global.ask { () =>
    val s = toSelection("""
      package demo

      class A

      object Demo{
        val m1 = {
          val hidden = 1
          hidden
        }

        def fn = {
          val a = 1
          /*(*/a/*)*/
        }

        val m2 = 2
      }

      trait C
      """)

    val innermost = ScopeTree.build(s.root, s.selectedTopLevelTrees.head)

    val valA = s.root.find(t => t.isInstanceOf[ValDef] && t.symbol.nameString == "a").get.symbol
    val defFn = s.root.find(t => t.isInstanceOf[DefDef] && t.symbol.nameString == "fn").get.symbol
    val valHidden = s.root.find(t => t.isInstanceOf[ValDef] && t.symbol.nameString == "hidden").get.symbol
    val traitC = s.root.find(t => t.isInstanceOf[ImplDef] && t.symbol.nameString == "C").get.symbol

    assertTrue(innermost.sees(valA))
    assertTrue(innermost.sees(defFn))
    assertTrue(innermost.sees(traitC))
    assertFalse(innermost.sees(valHidden))

    val outermost = innermost.outermostScope

    assertFalse(outermost.sees(defFn))
    assertTrue(innermost.sees(traitC))
  }

  @Test
  def visibilityOfImports() = global.ask { () =>
    val s = toSelection("""
      import scala.collection.mutable

      class Demo{
        import mutable.LinkedList

        def local = {
          import scala.math.Pi
          /*(*/(Pi, new LinkedList)/*)*/
        }

        import mutable.Set
      }
      """)

    val innermost = ScopeTree.build(s)

    val pi = s.inboundDeps.find(_.nameString == "Pi").get
    val linkedCtor = s.inboundDeps.find(_.toString == "constructor LinkedList").get

    assertTrue(innermost.sees(pi))
    assertTrue(innermost.sees(linkedCtor))
  }

  @Test
  @Ignore("Not yet supported")
  def visibilityOfInheritedMembers() = global.ask { () =>
    val s = toSelection("""
      trait Base{
        val baseVal = 1
      }

      object O extends Base {
        def fn = /*(*/baseVal/*)*/
      }
      """)

    val innermost = ScopeTree.build(s)

    val baseVal = s.inboundDeps.find(_.nameString == "baseVal").get

    assertTrue(innermost.sees(baseVal))

    val outermost = innermost.outermostScope

    assertFalse(outermost.sees(baseVal))
  }

  @Test
  def visibilityOfRenamedImport() = global.ask { () =>
    val s = toSelection("""
      import scala.collection.mutable

      class Demo{
        import mutable.{LinkedList => LL}

        def local = {
          /*(*/new LL/*)*/
        }
      }
      """)

    val innermost = ScopeTree.build(s)

    val consLinked = global.ask {
      () => s.inboundDeps.find(_.toString == "constructor LinkedList").get
    }

    assertTrue(innermost.sees(consLinked))
  }

  @Test
  def visibilityOfWildcardImport() = global.ask { () =>
    val s = toSelection("""
      import scala.collection.mutable

      class Demo{
        import mutable._

        def local = {
          /*(*/new LinkedList/*)*/
        }
      }
      """)

    val innermost = ScopeTree.build(s)
    val consLinked = global.ask { () =>
      s.inboundDeps.find(_.toString == "constructor LinkedList").get
    }

    assertTrue(innermost.sees(consLinked))
  }

  @Test
  def scopeLookup() = global.ask { () =>
    val s = toSelection("""
      object Demo{
        val a = 1

        val b = {
          val l = 123
          /*(*/2/*)*/
        }

        val c = 3
      }
    """)

    val scopes = ScopeTree.build(s.root, s.selectedTopLevelTrees.head)

    val valA = s.root.find(t => t.isInstanceOf[ValDef] && t.symbol.nameString == "a").get
    val valB = s.root.find(t => t.isInstanceOf[ValDef] && t.symbol.nameString == "b").get
    val valC = s.root.find(t => t.isInstanceOf[ValDef] && t.symbol.nameString == "c").get

    assertEquals("MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(valA).toString())
    assertEquals("MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(valB).toString())
    assertEquals("MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(valC).toString())
    assertEquals("LocalScope(l) -> MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(s.selectedTopLevelTrees.head).toString())
  }

  @Test
  def scopeLookupOfParams() = global.ask { () =>
    val s = toSelection("""
      class Demo(cp: Int){
        def fn(a: Int) = {
          val giveMeABlock = true
          (x: Int) => {
            /*(*/cp * a * x/*)*/
          }
        }
      }
    """)

    val scopes = ScopeTree.build(s.root, s.selectedTopLevelTrees.head)

    val block = s.findSelectedOfType[Block].get
    val defFn = s.findSelectedOfType[DefDef].get
    val template = s.findSelectedOfType[Template].get

    assertEquals("LocalScope(a) -> MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(block.children.head).toString())
    assertEquals("LocalScope(a) -> MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(block).toString())
    assertEquals("MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(defFn).toString())
    assertEquals("MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(template).toString())
  }

  @Test
  def scopeLookupOfParamsOfDesugaredFunctions() = global.ask { () =>
    val s = toSelection("""
      object Demo{
        for(i <- 1 to 10) /*(*/println(i)/*)*/
      }
    """)

    val scopes = ScopeTree.build(s.root, s.selectedTopLevelTrees.head)

    val template = s.findSelectedOfType[Template].get
    val applyForeach = template.children.collectFirst {
      case d: Apply if d.symbol.nameString == "foreach" => d
    }.get
    val applyPrintln = template.collect {
      case d: RefTree if d.symbol.nameString == "println" => d
    }.head

    assertEquals("MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(template).toString())
    assertEquals("MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(applyForeach).toString())
    assertEquals("LocalScope(i) -> MemberScope(Demo) -> MemberScope(<empty>)", scopes.findScopeFor(applyPrintln).toString())
  }

  @Test
  def scopeLookupInNestedClasses() = global.ask { () =>
    val s = toSelection("""
      class Outer {
        def fn(p: Int) = {
          class Inner {
            val a = /*(*/p/*)*/
          }
        }
      }
    """)

    val scopes = ScopeTree.build(s.root, s.selectedTopLevelTrees.head)

    val inner = s.findSelectedOfType[Template].get
    val outer = s.findSelectedWithPredicate {
      case t: Template if t != inner => true
      case _ => false
    }.get

    assertEquals("MemberScope(Outer) -> MemberScope(<empty>)", scopes.findScopeFor(outer).toString())
    assertEquals("MemberScope(Inner) -> LocalScope(p) -> MemberScope(Outer) -> MemberScope(<empty>)", scopes.findScopeFor(inner).toString())
  }

  @Test
  def scopeLookupInNestedClasses2() = global.ask { () =>
    val s = toSelection("""
      class Outer {
        class Inner {
          val a = 1
          println(/*(*/a/*)*/
        }
      }
    """)

    val scopes = ScopeTree.build(s.root, s.selectedTopLevelTrees.head)

    val inner = s.findSelectedOfType[Template].get
    val outer = s.findSelectedWithPredicate {
      case t: Template if t != inner => true
      case _ => false
    }.get

    val outerScope = scopes.findScopeFor(outer)

    assertEquals("MemberScope(Outer) -> MemberScope(<empty>)", outerScope.toString())
    assertEquals("MemberScope(Inner) -> MemberScope(Outer) -> MemberScope(<empty>)", scopes.toString())

    val aSym = s.inboundDeps.head

    assertTrue(scopes.sees(aSym))
    assertFalse(outerScope.sees(aSym))
  }
}
