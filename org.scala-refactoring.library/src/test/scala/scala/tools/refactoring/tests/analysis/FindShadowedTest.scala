/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import tests.util.TestHelper
import org.junit.Assert._
import analysis._

class FindShadowedTest extends TestHelper {

  import global._

  @Ignore
  @Test
  def findSimpleShadowing() {

    val t = treeFrom("""
    package shadowing

    object TheShadow {
      val i = 1

      def method {
        val i = ""
        ()
      }
    }

    class Xyz(xyzxyz: Long) {

      def method {
        val xyzxyz = ""
        val i = xyzxyz
        ()
      }
    }

    class Z {
      import TheShadow._

      val i = Nil
    }""")

    val results = new collection.mutable.ListBuffer[Symbol]

    t foreach {
      case v @ ValDef(_, name, _, _) =>

        val members = {

          def contexts(n: Context): Stream[Context] = n #:: contexts(n.outer)

          val context = global.doLocateContext(v.pos).outer

          val fromEnclosingScopes = contexts(context).takeWhile(_ != NoContext) flatMap {
            ctx => ctx.scope ++ (if (ctx == ctx.enclClass) ctx.prefix.members else Nil)
          }

          val fromImported = (context.imports flatMap (_.allImportedSymbols))

          fromEnclosingScopes ++ fromImported
        }

        results ++= members.find(s => s.name == name && s.pos != v.symbol.pos)

      case _ => Nil
    }

    assertEquals(3, results.size)
  }
}

