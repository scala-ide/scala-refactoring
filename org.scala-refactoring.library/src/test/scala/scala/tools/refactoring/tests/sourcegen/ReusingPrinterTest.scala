package scala.tools.refactoring.tests
package sourcegen

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.refactoring.common.SilentTracing
import scala.tools.refactoring.sourcegen.EmptyFragment
import scala.tools.refactoring.sourcegen.Fragment
import scala.tools.refactoring.tests.util.TestHelper

import org.junit.ComparisonFailure

class ReusingPrinterTest extends TestHelper with SilentTracing {

  import global._

  final override def print(t: Tree, ctx: PrintingContext): Fragment = {
    if(t.hasExistingCode)
      reusingPrinter.dispatchToPrinter(t, ctx)
    else if(t.hasNoCode)
      prettyPrinter.dispatchToPrinter(t, ctx)
    else
      EmptyFragment
  }

  final implicit class ImplicitTreeHelper(original: Tree) {
    def printsTo(expectedOutput: String): Unit = {
      val sourceFile = new BatchSourceFile("noname", expectedOutput)
      val expected = stripWhitespacePreservers(expectedOutput).trim()
      val actual = ask { () => generate(original, sourceFile = Some(sourceFile)).asText.trim() }
      if (actual != expected)
        throw new ComparisonFailure("", expected, actual)
    }
  }
  final implicit class ToBecome(input: String) {
    def becomes(expectedOutput: String) = input -> expectedOutput
  }
  final implicit class OrToDieAfter(input: (String, String)) {
    def after(trans: Transformation[Tree, Tree]): Unit = {
      val t = ask { () => trans(treeFrom(input._1)) }
      require(t.isDefined, "transformation was not successful")
      t foreach (_.printsTo(input._2))
    }
  }

  @Test
  def add_return_type_to_def() = global.ask { () => """
    package add_return_type_to_def
    object X {
      def value = new java.io.File("")
    }""" becomes """
    package add_return_type_to_def
    object X {
      def value: java.io.File = new java.io.File("")
    }""" after topdown { matchingChildren { transform {
      case d @ DefDef(_, _, _, _, tpt: TypeTree, _) =>
        val newTpt = tpt setOriginal mkReturn(List(tpt.tpe.typeSymbol))
        d.copy(tpt = newTpt) replaces d
    }}}
  }

  @Test
  def add_return_type_to_val() = global.ask { () => """
    package add_return_type_to_val
    object X {
      val value = new java.io.File("")
    }""" becomes """
    package add_return_type_to_val
    object X {
      val value: java.io.File = new java.io.File("")
    }
    """ after topdown { matchingChildren { transform {
      case d @ ValDef(_, _, tpt: TypeTree, _) =>
        val newTpt = tpt setOriginal mkReturn(List(tpt.tpe.typeSymbol))
        d.copy(tpt = newTpt) replaces d
    }}}
  }

  @Test
  def add_return_type_to_var() = global.ask { () => """
    package add_return_type_to_var
    object X {
      var variable = new java.io.File("")
    }""" becomes """
    package add_return_type_to_var
    object X {
      var variable: java.io.File = new java.io.File("")
    }
    """ after topdown { matchingChildren { transform {
      case d @ ValDef(_, _, tpt: TypeTree, _) =>
        val newTpt = tpt setOriginal mkReturn(List(tpt.tpe.typeSymbol))
        d.copy(tpt = newTpt) replaces d
    }}}
  }

  @Test
  def add_override_flag() = global.ask { () => """
    package add_override_flag
    trait T {
      def meth: Int
    }
    trait TT extends T {
      def meth = 0
    }
    """ becomes """
    package add_override_flag
    trait T {
      def meth: Int
    }
    trait TT extends T {
      override def meth = 0
    }
    """ after topdown { matchingChildren {
      filter {
        case d: DefDef =>
          d.symbol.isOverridingSymbol && !d.symbol.isOverride
      } &>
      transform {
        case d: DefDef =>
          d.copy(mods = d.mods.withFlag(Flag.OVERRIDE)) replaces d
      }
    }}
  }

  @Test
  def add_override_final_flags_to_lazy_val() = global.ask { () => """
    package add_override_final_flags_to_lazy_val
    trait T {
      def meth: Int
    }
    trait TT extends T {
      lazy val meth = 0
    }
    """ becomes """
    package add_override_final_flags_to_lazy_val
    trait T {
      def meth: Int
    }
    trait TT extends T {
      override final lazy val meth = 0
    }
    """ after topdown { matchingChildren {
      filter {
        case d: DefDef =>
          d.symbol.isLazy
      } &>
      transform {
        case d: DefDef =>
          d.copy(mods = d.mods.withFlag(Flag.OVERRIDE).withFlag(Flag.FINAL).withFlag(Flag.LAZY).withFlag(Tokens.VAL)) replaces d
      }
    }}
  }

  @Test
  def add_override_protected_abstract_flag() = global.ask { () => """
    package add_override_protected_abstract_flag
    trait T {
      protected def meth: Int = 0
    }
    trait TT extends T {
      def meth = super.meth + 0
    }
    """ becomes """
    package add_override_protected_abstract_flag
    trait T {
      protected def meth: Int = 0
    }
    trait TT extends T {
      override protected abstract def meth = super.meth + 0
    }
    """ after topdown { matchingChildren {
      filter {
        case d: DefDef =>
          d.symbol.isOverridingSymbol && !d.symbol.isOverride
      } &>
      transform {
        case d: DefDef =>
          d.copy(mods = d.mods.withFlag(Flag.ABSTRACT).withFlag(Flag.OVERRIDE).withFlag(Flag.PROTECTED)) replaces d
      }
    }}
  }

  @Test
  def add_final_case_flag() = global.ask { () => """
    package add_final_case_flag
    class C(i: Int)
    """ becomes """
    package add_final_case_flag
    private final case class C(i: Int)
    """ after topdown { matchingChildren {
      transform {
        case d: ClassDef =>
          d.copy(mods = d.mods.withFlag(Flag.FINAL).withFlag(Flag.CASE).withFlag(Flag.PRIVATE)) replaces d
      }
    }}
  }

  @Test
  def add_modifier_to_def_without_return_type() = global.ask { () => """
    package add_modifier_to_def_without_return_type
    trait T {
      def meth: Int
    }
    trait TT extends T {
      def meth
    }
    """ becomes """
    package add_modifier_to_def_without_return_type
    trait T {
      def meth: Int
    }
    trait TT extends T {
      override def meth
    }
    """ after topdown { matchingChildren {
      filter {
        case d: DefDef =>
          d.symbol.isOverridingSymbol && !d.symbol.isOverride
      } &>
      transform {
        case d: DefDef =>
          d.copy(mods = d.mods.withFlag(Flag.OVERRIDE)) replaces d
      }
    }}
  }

  @Test
  def add_modifier_to_val_without_return_type() = global.ask { () => """
    package add_modifier_to_val_without_return_type
    trait T {
      def meth: Int
    }
    trait TT extends T {
      val meth
    }
    """ becomes """
    package add_modifier_to_val_without_return_type
    trait T {
      def meth: Int
    }
    trait TT extends T {
      override val meth
    }
    """ after topdown { matchingChildren {
      filter {
        case d: ValDef =>
          val getter = d.symbol.getter(d.symbol.owner)
          getter.isOverridingSymbol && !getter.isOverride
      } &>
      transform {
        case d: ValDef =>
          d.copy(mods = d.mods.withFlag(Flag.OVERRIDE)) replaces d
      }
    }}
  }
}
