/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import tests.util._
import org.junit.Assert._
import analysis._

class DeclarationIndexTest extends TestHelper with GlobalIndexes with TreeAnalysis with FreshCompilerForeachTest {

  import global._

  var index: IndexLookup = null

  def mapAndCompareSelectedTrees(expected: String, src: String)(m: PartialFunction[Tree, String]) = {

    // because we assert one file offsets, we don't want to have any windows newlines in the code
    val testSource = src.replaceAll("\r\n", "\n")

    val tree = treeFrom(testSource)

    index = GlobalIndex(List(CompilationUnitIndex(tree)))

    val firstSelected = {
      val start = commentSelectionStart(src)
      val end   = commentSelectionEnd(src)
      FileSelection(tree.pos.source.file, tree, start, end)
    }.selectedTopLevelTrees.head

    if(m.isDefinedAt(firstSelected)) {
      val result = m(firstSelected)
      assertEquals(expected, result)
    } else {
      throw new Exception("found: "+ firstSelected + "(" + firstSelected.getClass.getSimpleName + ")")
    }
  }

  def assertDeclarationOfSelection(expected: String, src: String) = global.ask{ () =>
    mapAndCompareSelectedTrees(expected, src) {
      case t @ (_: TypeTree | _: RefTree) =>
        index.declaration(t.symbol).head.toString
    }
  }

  def assertReferencesOfSelection(expected: String, src: String) = global.ask{ () =>
    mapAndCompareSelectedTrees(expected, src) {

      def refs(s: Symbol): String = {
         val ranges = index.references(s).toList filter (_.pos.isRange) sortBy(_.pos.start)
         val strings = ranges map ( ref => ref.toString +" ("+ ref.pos.start +", "+ ref.pos.end +")" )
         strings mkString ", "
      }

      t => t match {
        case t @ (_: TypeTree | _: DefTree) =>
          refs(t.symbol)
        case t: Ident =>
          val syms = index.positionToSymbol(t.pos)
          val allRefs = syms map refs
          allRefs.distinct mkString ", "
      }
    }
  }

  @Test
  def findValReference() = {
    assertDeclarationOfSelection("private[this] val x: Int = 1", """
      object AfindValReference {
        private[this] val x = 1
        val y = /*(*/  x  /*)*/
      }
      """)
  }

  @Test
  def findValReferenceFromMethod() = {
    assertDeclarationOfSelection("private[this] val x: Int = 1", """
      object AfindValReferenceFromMethod {
        private[this] val x = 1
        def go {
          val y = /*(*/  x  /*)*/
        }
      }
      """)
  }

  @Test
  def findShadowed() = {
    assertDeclarationOfSelection("""val x: String = "a"""", """
      object AfindShadowed {
        private[this] val x = 1
        def go  = {
          val x = "a"
          val y = /*(*/  x  /*)*/
          y
        }
      }
      """)
  }

  @Test
  def findShadowedWithThis() = {
    assertDeclarationOfSelection("""private[this] val x: Int = 1""", """
      object AfindShadowedWithThis {
        private[this] val x = 1
        def go = {
          val x = "a"
         /*(*/  this.x  /*)*/
        }
      }
      """)
  }

  @Test
  def findMethod() = {
    assertDeclarationOfSelection("""def x(): Int = 5""", """
      object AfindMethod {
        def x() = 5
        def go  = {
          val y = /*(*/  x  /*)*/ ()
          y
        }
      }
      """)
  }

  @Test
  def findMethodFromOtherClass() = {
    assertDeclarationOfSelection("""def x: Int = 5""", """
    package findMethodFromOtherClass {
      class N {
        def x: Int = 5
      }
      object M {
        def go  = {
          val a = new N
          val y = /*(*/  a.x  /*)*/
          y
        }
      }
    }
      """)
  }

  @Test
  def findReferencesToLocal() = {
    assertReferencesOfSelection("a (86, 87), a (98, 99)", """
      class H {
        def go  = {
 /*(*/    val a = 5      /*)*/
          val y = a
          a
        }
      }
      """)
  }

  @Test
  def findReferencesToMethod() = {
    assertReferencesOfSelection("""G.this.go (89, 91)""", """
      class G {
 /*(*/
        def go() = {
          5
        } /*)*/
        val g = go()
      }

      """)
  }

  @Test
  def findReferencesToSuperConstructorParameter() = {
    assertReferencesOfSelection("""a (81, 82)""", """

      class Base(s: String)

      class Sub(/*(*/a: String/*)*/) extends Base(a)
      """)
  }

  @Test
  def findReferencesToClass() = {
    assertReferencesOfSelection("""Z (67, 68), Z (87, 88), Z (111, 112), Z (115, 116), Z (123, 124)""", """
      package xyz

 /*(*/  class Z   /*)*/

      class B extends Z

      class C(a: Z) {
        def get(a: Z): Z = new Z
      }
      """)
  }

  @Test
  def findClassDeclarationToMethodParameter() = {
    assertReferencesOfSelection("""scala.this.Predef.String (53, 59), scala.this.Predef.String (85, 91)""", """
      class Xy

      object I {
        def go(xy: String) = {
          xy: /*(*/ String /*)*/
        }
      }
      """)
  }

  @Test
  def findClassDeclarationFromMethodParameter() = {
    assertReferencesOfSelection("""scala.this.Predef.String (59, 65), scala.this.Predef.String (91, 97)""", """
      class Xy

      object J {
        def go(xy: /*(*/ String /*)*/) = {
          xy: String
        }
      }
      """)
  }

  @Test
  def referencesToLazyVal() = {
    val tree =  """
      object L {
        def go = {
          lazy val x = 5

          /*(*/ x /*)*/
        }
      }
      """
    assertReferencesOfSelection("""x (79, 80)""", tree)
    assertDeclarationOfSelection("""<stable> <accessor> lazy def x: Int = {
  x$lzy = 5;
  x$lzy
}""", tree)
  }

  @Test
  def referencesToTypes() = {
    val tree =  """
      object L {
        def go[T](t: /*(*/ T /*)*/) = {
          t: T
        }
      }
      """
    assertReferencesOfSelection("""T (45, 46), T (71, 72)""", tree)
    assertDeclarationOfSelection("""type T""", tree)
  }

  @Test
  def referencesToTypesInAppliedTypes() = {
    assertReferencesOfSelection("""scala.this.Predef.String (41, 47), scala.this.Predef.String (76, 82), scala.this.Predef.String (111, 117)""", """
      object U {
        def go(t: List[String]) = {
          val s: /*(*/String/*)*/ = ""
          t: List[String]
        }
      }
      """)
  }

  @Test
  def referencesToValueInForComprehensionFilter() = {
    assertReferencesOfSelection("""foo (74, 77), foo (101, 104)""", """
      object U {
        for (/*(*/foo/*)*/ <- List("santa", "claus") if foo.startsWith("s")) yield foo
      }
      """)
  }

  @Test
  def referencesToClassWithSuper() = {
    assertReferencesOfSelection("""""", """
      class T {
        /*(*/private abstract class MySource extends Iterator[String] {
          override def hasNext = super.hasNext
        }/*)*/
      }
      """)
  }
}

