package scala.tools.refactoring.tests

import scala.tools.refactoring.util.Tracing
import scala.tools.refactoring.util.SilentTracing
import scala.tools.refactoring.OrganizeImports
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class OrganizeImportsTest extends TestHelper {
  
  class StringExtractMethod(source: String) {
    def organize(expected: String) = {
      val refactoring = new OrganizeImports(global) with Tracing
      refactoring.prepare(compile(source), 0, 0) match {
        case Right(prepare) =>
          val result = refactoring.perform(prepare, new refactoring.RefactoringParameters) match {
            case Right(result) => result
            case Left(error) => error
          }
          assertEquals(expected, result)
        case Left(error) => fail()
      }
    }
  }
  
  implicit def stringToStringExtractMethod(source: String) = new StringExtractMethod(source)

  @Test
  def sort = """
    import scala.collection.mutable.ListBuffer
    import java.lang.Object

    object Main
    """ organize(
    """
    import java.lang.Object
    import scala.collection.mutable.ListBuffer

    object Main
    """)
    
  @Test
  def collapse = """
    import java.lang.String
    import java.lang.Object

    object Main
    """ organize(
    """
    import java.lang.{Object, String}

    object Main
    """)    
    
  @Test
  def sortAndCollapse = """
    import scala.collection.mutable.ListBuffer
    import java.lang.String
    import java.lang.Object

    object Main
    """ organize(
    """
    import java.lang.{Object, String}
    import scala.collection.mutable.ListBuffer

    object Main
    """)    
    
  @Test
  def collapseWithRename = """
    import java.lang.{String => S}
    import java.lang.{Object => O}

    object Main
    """ organize(
    """
    import java.lang.{Object => O, String => S}

    object Main
    """)     
    
  @Test
  def importAll = """
    import java.lang._
    import java.lang.String

    object Main
    """ organize(
    """
    import java.lang._

    object Main
    """)    
    
  @Test
  def importWithSpace = """
    package test

    import scala.collection.mutable.ListBuffer
    import java.lang.String

    object Main
    """ organize(
    """
    package test

    import java.lang.String
    import scala.collection.mutable.ListBuffer

    object Main
    """)    
    
  @Test
  def importAllWithRename = """
    import java.lang._
    import java.lang.{String => S}

    object Main
    """ organize(
    """
    import java.lang.{String => S, _}

    object Main
    """)
    
  @Test
  def large = """
    package untyped
    
    import scala.util.parsing.combinator._
    
    sealed abstract class Term
    case class  TmVar(x: Int, n: Int) extends Term
    case class  TmAbs(x: String, t: Term) extends Term
    case class  TmApp(t1: Term, t2: Term) extends Term
    
    class Binding {
      override def toString = "þ"
    }
    
    sealed abstract class Command
    case class Eval(t: Term) extends Command
    case class Bind(x: String, binding: Binding) extends Command
    
    class UntypedParser extends JavaTokenParsers with PackratParsers {
      
      def name2index(ctx: Context, x: String): Int = ctx match {
        case Nil => throw new Exception("Identifier "+ x +" is unbound in context "+ (ctx mkString ", "))
        case (y, _) :: ys if x == y => 0
        case (y, _) :: ys => 1 + name2index(ys, x)
      }
      
      def index2name(ctx: Context, x: Int) = ctx(x) _1
      
      def addname(ctx: Context, x: String): Context = (x, new Binding) :: ctx
      
      type Context = List[(String, Binding)]
      
      lazy val toplevel = repsep(command, ";")<~";" ^^ (cmds => (ctx: Context) => cmds.foldLeft((Nil: List[Command], ctx)) {
        case ((cmds, ctx), res) => res(ctx) match {case(cmd, newCtx) => (cmd :: cmds, newCtx)}
      })
      
      lazy val command: PackratParser[Context => (Command, Context)] = 
        LCID<~"/" ^^ (lcid => (ctx: Context) => (Bind(lcid, new Binding), addname(ctx, lcid)) ) |
        Term ^^ (t => (ctx: Context) => (Eval(t(ctx)), ctx))
      
      def LCID = "\\w".r
        
      lazy val Term: PackratParser[Context => Term] = 
        "lambda"~LCID~"."~Term ^^ {case "lambda"~id~"."~t => (ctx: Context) => TmAbs(id, t(addname(ctx, id)))} |
        AppTerm
      
      lazy val AppTerm: PackratParser[Context => Term] = 
        AppTerm~ATerm ^^ {case t1~t2 => (ctx: Context) => TmApp(t1(ctx), t2(ctx))} |
        ATerm
        
      lazy val ATerm: PackratParser[Context => Term] = 
        "("~>Term<~")" |
        LCID ^^ (lcid => (ctx: Context) => TmVar(name2index(ctx, lcid), ctx.length))
    }
    
    object Untyped extends UntypedParser {
      
      def main(args : Array[String]) {
            
        println(parseAll(toplevel, "x/;x;") map (_(Nil)) )
        println(parseAll(toplevel, "lambda x. x;") map (_(Nil)))
        println(parseAll(toplevel, "(lambda x. x);") map (_(Nil)))
        println(parseAll(toplevel, "(lambda x. x) (lambda x. x x);") map (_(Nil)))
        println(parseAll(toplevel, "((lambda x. x) (lambda x. x x)) (lambda x. x x);") map (_(Nil)))
        println(parseAll(toplevel, "(lambda x. x) (lambda x. x x) (lambda x. x x);") map (_(Nil)))
        
        println("===")
        
        println(parseAll(toplevel, "(lambda x. (lambda y. y x));") map (_(Nil)))
    
      }
    }
    """organize(
    """""")
}
