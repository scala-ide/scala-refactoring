/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.sourcegen

import tests.util.TestHelper
import org.junit.Assert
import org.junit.Assert._
import sourcegen.SourceGenerator
import common.SilentTracing
import common.ConsoleTracing
import tools.nsc.symtab.Flags
import tools.nsc.ast.parser.Tokens
import scala.reflect.internal.util.BatchSourceFile

class PrettyPrinterTest extends TestHelper with SilentTracing {

  import global._

  implicit class TreePrettyPrintMethods(original: Tree) {
    def prettyPrintsTo(expected: String) = global.ask { () =>
      val sourceFile = {
        // we only need the source file to see what kinds of newline we need to generate,
        // so we just pass the expected output :-)
        new BatchSourceFile("noname", expected)
      }
      assertEquals(stripWhitespacePreservers(expected), generate(cleanTree(original), sourceFile = Some(sourceFile)).asText)
    }
  }

  val changeSomeModifiers = transform {
    case t: ClassDef =>
      t.copy(mods = NoMods) setPos t.pos
    case t: DefDef =>
      t.copy(mods = NoMods withPosition (Flags.PROTECTED, NoPosition) withPosition (Flags.METHOD, NoPosition)) setPos t.pos
    case t: ValDef =>
      t.copy(mods = NoMods withPosition (Tokens.VAL, NoPosition)) setPos t.pos
    case t => t
  }

  @Test
  def testMethodDocDef() = {

    val doc = """/**
 * Bla
 *
 * Bla bla
 * /
"""
    val method = DocDef(DocComment(doc, NoPosition),mkDefDef(name = "meth", body = DocDef(DocComment("/** Kuuka */", NoPosition), EmptyTree) :: Ident(newTermName("()")) :: Nil))

    val tree = mkCaseClass(
        name = "A",
        body = method :: Nil)

    tree prettyPrintsTo """case class A {
  /**
   * Bla
   *
   * Bla bla
   * /
  def meth() = {
    /** Kuuka */
    ()
  }
}"""
  }

  @Test
  def testCaseClassNoArgList() {
    mkCaseClass(name = "A", argss = Nil) prettyPrintsTo "case class A"
  }

  @Test
  def testCaseClassTwoArgLists() {
    val argsList1 = (NoMods, "r1", Ident(newTermName("Rate"))) :: Nil
    val argsList2 = (NoMods, "r2", Ident(newTermName("Rate"))) :: (NoMods, "r3", Ident(newTermName("Rate"))) :: Nil

    mkCaseClass(name = "A", argss = List(argsList1, argsList2)) prettyPrintsTo "case class A(r1: Rate)(r2: Rate, r3: Rate)"
  }

  @Test
  def testCaseClassZeroArgs() {
    mkCaseClass(name = "A", argss = Nil :: Nil) prettyPrintsTo "case class A()"
  }

  @Test
  def testCaseClassOneArg() = {
    mkCaseClass(
        name = "A",
        argss = ((NoMods, "rate", Ident(newTermName("Rate"))) :: Nil) :: Nil) prettyPrintsTo "case class A(rate: Rate)"
  }

  @Test
  def testCaseClassTwoArgs() = {
    mkCaseClass(
        name = "A",
        argss = List(List((NoMods, "x", Ident(newTermName("Int"))), (NoMods, "y", Ident(newTermName("String")))))) prettyPrintsTo "case class A(x: Int, y: String)"
  }

  @Test
  def testClassTwoArgs() = {
    mkClass(
        name = "A",
        argss = List(List((NoMods, "x", Ident(newTermName("Int"))), (NoMods, "y", Ident(newTermName("String")))))) prettyPrintsTo "class A(x: Int, y: String)"
  }

  @Test
  def testClassTwoValVarArgs() = {
    mkClass(
        name = "A",
        argss = List(List((NoMods withPosition (Tokens.VAL, NoPosition), "x", Ident(newTermName("Int"))), (NoMods withPosition (Tokens.VAR, NoPosition), "y", Ident(newTermName("String")))))) prettyPrintsTo "class A(val x: Int, var y: String)"
  }

  @Test
  def testSuperCall =  {

    val tree = mkCaseClass(
        name = "A",
        argss = ((NoMods, "x", Ident(newTermName("Int"))) :: Nil) :: Nil,
        parents = Ident(newTermName("X")) :: Ident(newTermName("Y")) :: Nil)

    tree prettyPrintsTo "case class A(x: Int) extends X with Y"
  }

  @Test
  def testSuperConstructorCall =  {

    val tree = mkCaseClass(
        name = "A",
        argss = ((NoMods, "x", Ident(newTermName("Int"))) :: Nil) :: Nil,
        parents = Ident(newTermName("X")) :: Nil,
        superArgs = Ident(newTermName("x")) :: Nil)

    tree prettyPrintsTo "case class A(x: Int) extends X(x)"
  }

  @Test
  def testDefDefWithoutParens =  {

    val tree = Block(
        DefDef(
          NoMods withPosition (Flags.METHOD, NoPosition),
          newTermName("eins"),
          Nil,
          Nil,
          EmptyTree,
          Literal(Constant(()))
        ),
        DefDef(
          NoMods withPosition (Flags.METHOD, NoPosition),
          newTermName("zwei"),
          Nil,
          Nil :: Nil,
          EmptyTree,
          Literal(Constant(()))
        ))

    tree prettyPrintsTo """{
  def eins = ()
  def zwei() = ()
}"""
  }

  @Test
  def testImplicitKeyword =  {

    val tree = DefDef(
          NoMods withPosition (Flags.IMPLICIT, NoPosition) withPosition (Flags.METHOD, NoPosition) ,
          newTermName("eins"),
          Nil,
          List(List(ValDef(NoMods withPosition (Flags.IMPLICIT, NoPosition), newTermName("a"), EmptyTree, EmptyTree))),
          EmptyTree,
          Literal(Constant(()))
        )

    tree prettyPrintsTo """implicit def eins(implicit val a) = ()"""
  }

  @Test
  def testDefDefWithTypeParams =  {

    val arg = ValDef(NoMods withPosition (Flags.IMPLICIT, NoPosition), newTermName("a"),
                TypeDef(NoMods, newTypeName("R"), TypeDef(NoMods, newTypeName("X"), Nil, EmptyTree) :: Nil, EmptyTree), EmptyTree)

    val tree = DefDef(
          NoMods withPosition (Flags.METHOD, NoPosition) ,
          newTermName("m"),
          TypeDef(NoMods, newTypeName("X"), Nil, EmptyTree) :: Nil,
          List(List(arg)),
          EmptyTree,
          Literal(Constant(()))
        )

    tree prettyPrintsTo """def m[X](implicit val a: R[X]) = ()"""
  }

  @Test
  def testDocDef() = {

    val doc = DocDef(DocComment("/** Kuuka */", NoPosition), EmptyTree)

    val tree = mkDefDef(name = "meth", body = doc :: Ident(newTermName("()")) :: Nil)

    tree prettyPrintsTo """def meth() = {
  /** Kuuka */
  ()
}"""
  }

  @Test
  def testApplyHasParens() = {
    Apply(Ident(newTermName("aa")), Nil) prettyPrintsTo """aa()"""
  }

  @Test
  def testApplyTypesToClass() = {
    TypeApply(Ident(newTermName("MyClass")), Ident(newTermName("A")) :: Ident(newTermName("B")) :: Nil) prettyPrintsTo """MyClass[A, B]"""
  }

  @Test
  def testClassWithTypeParams() = {
    val c = mkClass(name = "A", tparams = List(TypeDef(NoMods, newTypeName("T"), Nil, EmptyTree), TypeDef(NoMods, newTypeName("U"), Nil, EmptyTree)))
    c prettyPrintsTo """class A[T, U]"""
  }

  @Test
  def testFloatLiteralFromIdent() = {
   Ident(newTermName("33.3f")) prettyPrintsTo """33.3f"""
  }

  @Test
  def testNumericLiterals() = {
   Literal(Constant(33.3f)) prettyPrintsTo """33.3f"""
   Literal(Constant(33.3d)) prettyPrintsTo """33.3"""
   Literal(Constant(33)) prettyPrintsTo """33"""
   Literal(Constant(33l)) prettyPrintsTo """33l"""
  }

  @Test
  def testUpperBound() = {

    val tree = DefDef(
          NoMods withPosition (Flags.METHOD, NoPosition),
          newTermName("eins"),
          TypeDef( NoMods, newTypeName("R"), Nil, TypeBoundsTree( EmptyTree, TypeDef( NoMods, newTypeName("Rate"), Nil, EmptyTree ))) :: Nil,
          Nil,
          EmptyTree,
          Literal(Constant(()))
        )

    tree prettyPrintsTo """def eins[R <: Rate] = ()"""
  }

  @Test
  def testNew() = {

    val tree = treeFrom("""
    object Functions {
      val a = new String("hello")
      def createNew = new {
        println("hello from an anonymous class")
      }
    }
    """)

    // terrible, but don't know how to do better :/
    tree prettyPrintsTo """object Functions {
  val a = new String("hello")

  def createNew = {
    class $anon {
      println("hello from an anonymous class")
    }
    new ()
  }
}"""
  }

  @Test
  def testThrow() = {

    val tree = treeFrom("""
    class Throw1 {
      throw new Exception("hu!")
    }

    class Throw2 {
      var msg = "   "
      val e = new Exception(msg)
      throw e
    }
    """)

    tree prettyPrintsTo """class Throw1 {
  throw new Exception("hu!")
}

class Throw2 {
  var msg = "   "

  val e = new Exception(msg)

  throw e
}"""
  }

  @Test
  def testAnnotation() = {

    val tree = treeFrom("""
    import scala.reflect.BeanProperty
    class ATest {
      @BeanProperty
      var status = ""
    }

    """)

    // XXX annotation is missing
    tree prettyPrintsTo """import scala.reflect.BeanProperty

class ATest {
  var status = ""
}"""
  }

  @Test
  def allNeededParenthesesArePrinted() = {

    val tree = treeFrom("""
    class Test {
      val x = true && !(true && false)
    }
    """)

    tree prettyPrintsTo """class Test {
  val x = true && true && false.!
}"""
  }

  @Test
  def multipleAssignmentWithTuple() = {
    treeFrom("""
    class Test {
      val (a, b) = (1, 2)
    }
    """) prettyPrintsTo """class Test {
  val (a, b) = (1, 2)
}"""
  }

  @Test
  def multipleAssignmentWithPimpedTuple() = {
    treeFrom("""
    class Test {
      val (a, b) = 1 -> 2
    }
    """) prettyPrintsTo """class Test {
  val (a, b) = (1).->(2)
}"""
  }

  @Test
  def multipleAssignmentWith4Tuple() = {
    treeFrom("""
    class Test {
      val (c, d, e, f) = (1, 2, 3, 4)
    }
    """) prettyPrintsTo """class Test {
  val (c, d, e, f) = (1, 2, 3, 4)
}"""
  }

  @Test
  def multipleAssignmentFromMethodResult() = {

    val tree = treeFrom("""
    class Test {
      val (g, h, i) = inMethod()

      def inMethod() = {
        println("in method")
        val (a, b, c) = (1, 2, 3)
        println("in method")
        (a, b, c)
      }
    }
    """)

    tree prettyPrintsTo """class Test {
  val (g, h, i) = inMethod()

  def inMethod() = {
    println("in method")
    val (a, b, c) = (1, 2, 3)
    println("in method")
    (a, b, c)
  }
}"""
  }

  @Test
  def patternMatchInAssignment() = {

    val tree = treeFrom("""
    class Test {
      val List(one, three, eight) = List(1,3,8)
    }
    """)

    tree prettyPrintsTo """class Test {
  val List(one, three, eight) = List(1, 3, 8)
}"""
  }

  @Test
  def testExistential() = {

    val tree = treeFrom("""
    class A(l: List[_])

    class B(l: List[T] forSome { type T })
    """)

    tree prettyPrintsTo """class A(l: List[_])

class B(l: List[T] forSome {type T})"""
  }

  @Test
  def testMissingParentheses() = {

    val tree = treeFrom("""
    package com.somedomain.test

    object Transaction {
      object Kind {
        val ReadOnly = true
      }
      def run[T](readOnyl: Boolean) = ()
    }

    object Test44 {
       def doNothing {
       }
    }
    class Test44 {
       def bar() {
         Transaction.run[Unit](Transaction.Kind.ReadOnly)
       }
    }""")

    tree prettyPrintsTo """package com.somedomain.test

object Transaction {
  object Kind {
    val ReadOnly = true
  }

  def run[T](readOnyl: Boolean) = ()
}

object Test44 {
  def doNothing = ()
}

class Test44 {
  def bar() = Transaction.run[Unit](Transaction.Kind.ReadOnly)
}"""
  }

  @Test
  def testCompoundTypeTree() = {

    val tree = treeFrom("""
    trait A
    trait B
    abstract class C(val a: A with B) {
      def method(x: A with B with C {val x: Int}): A with B
    }
    """)

    tree prettyPrintsTo """trait A

trait B

abstract class C(val a: A with B) {
  def method(x: A with B with C {
    val x: Int
  }): A with B
}"""
  }

  @Test
  def testSingletonTypeTree() = {

    val tree = treeFrom("""
    trait A {
      def doSomething(): this.type
    }
    """)

    tree prettyPrintsTo """trait A {
  def doSomething(): this.type
}"""
  }

  @Test
  def testSelectFromTypeTree() = {

    val tree = treeFrom("""
    trait A {
      type T
    }

    class B(t: A#T)
    """)

    tree prettyPrintsTo """trait A {
  type T
}

class B(t: A#T)"""
  }

  @Ignore
  @Test
  def testSelfTypesWithThis() = {

    val tree = treeFrom("""
    package common {
      trait Tracing
      trait PimpedTrees
    }

    trait AbstractPrinter {
      this: common.Tracing with common.PimpedTrees =>
    }
    """)

    // XXX wrong!
    tree prettyPrintsTo """package common

trait Tracing

trait PimpedTrees

trait AbstractPrinter {
  this: common.Tracing with common.PimpedTrees =>
}"""
  }

  @Test
  def testWhileLoop() = {

    val tree = treeFrom("""
    trait WhileLoop {
      while/*a*/(true != false) println("The world is still ok!")

      while(true != false) {
        println("The world is still ok!")
      }

      while(true) {
        println("The world is still ok!")
        println("The world is still ok!")
      }

      while(true) {
        println("The world is still ok!")
        println("The world is still ok!")
        println("The world is still ok!")
        println("The world is still ok!")
        println("The world is still ok!")
        println("The world is still ok!")
      }
    }
    """)

    tree prettyPrintsTo """trait WhileLoop {
  while(true != false){
    println("The world is still ok!")
  }

  while(true != false){
    println("The world is still ok!")
  }

  while(true){
    println("The world is still ok!")
    println("The world is still ok!")
  }

  while(true){
    println("The world is still ok!")
    println("The world is still ok!")
    println("The world is still ok!")
    println("The world is still ok!")
    println("The world is still ok!")
    println("The world is still ok!")
  }
}"""
  }

  @Test
  def testDoWhileLoop() = {

    val tree = treeFrom("""
    trait WhileLoop {
      do println("The world is still ok!") while (true)

      do {
        println("The world is still ok!")
      } while(true != false)

      do {
        println("The world is still ok!")
        println("The world is still ok!")
      } while(true)
    }
    """)

    tree prettyPrintsTo """trait WhileLoop {
  do println("The world is still ok!") while(true)

  do println("The world is still ok!") while(true != false)

  do {
    println("The world is still ok!")
    println("The world is still ok!")
  } while(true)
}"""
  }

  @Test
  def testPlusEquals() = {
    val tree = treeFrom("""
      trait Demo2 {
        var assignee = 1
        assignee += -42
      }""")

    //TODO fixme tree prettyPrintsTo """"""
  }

  @Test
  def testAssign() = {
    val tree = treeFrom("""
      trait Demo1 {
        def method {
          var i = 0
          i = 1
        }
      }""")

    tree prettyPrintsTo """trait Demo1 {
  def method = {
    var i = 0
    i = 1
  }
}"""
  }

  @Test
  def testSetters() = {
    val tree = treeFrom("""
      package oneFromMany
      class Demo(val a: String,  /*(*/private var _i: Int/*)*/  ) {
        def i_=(i: Int) = {
          _i = i
        }
      }""")

    tree prettyPrintsTo """package oneFromMany

class Demo(val a: String, private var _i: Int) {
  def i_=(i: Int) = _i = i
}"""
  }

  @Test
  def typeParametersAreSeparatedByComma() = {
    val tree = treeFrom("""
      class MethodWithTypeParam {
        def foo[A, B, C] = 1
      }""")

    tree prettyPrintsTo """class MethodWithTypeParam {
  def foo[A, B, C] = 1
}"""
  }

  @Test
  def testClassConstructorParams() = {
    val tree = treeFrom("""
      class Demo1(a: String, b: Int)
      class Demo2(a: String, b: Int)
    """)

    tree prettyPrintsTo """class Demo1(a: String, b: Int)

class Demo2(a: String, b: Int)"""
  }

  @Test
  def testMatches() = {
    val tree = treeFrom("""
    object Functions {
      List(1,2) match {
        case i => i
      }

      List(1,2) map {
        case i if i > 5 => i
      }

      List(1,2) map {
        case i: Int => i
      }

      List(1,2) map {
        case a @ (i: Int) => i
      }

      List(1,2) map {
        case _ => 42
      }

      List(1,2) match {
        case x :: xs => x
      }

      List(1,2) map {
        case 0 | 1 => true
        case _ => false
      }
    }
    """)
    // FIXME problem with ::
    tree prettyPrintsTo """object Functions {
  List(1, 2) match {
    case i => i
  }

  List(1, 2).map {
    case i if i > 5 => i
  }

  List(1, 2).map {
    case i: Int => i
  }

  List(1, 2).map {
    case a @ (i: Int) => i
  }

  List(1, 2).map {
    case _ => 42
  }

  List(1, 2) match {
    case (x, xs) => x
  }

  List(1, 2).map {
    case 0 | 1 => true
    case _ => false
  }
}"""
  }

  @Test
  def testReturn() = {
    val tree = treeFrom("""
    object Functions {
      def test: Int = {
        return 5
      }
    }
    """)

    tree prettyPrintsTo """object Functions {
  def test: Int = return 5
}"""
  }

  @Test
  def testVarArgs() = {
    val tree = treeFrom("""
    object Functions {
      def test(args: String*) = args.toList
    }
    """)

    tree prettyPrintsTo """object Functions {
  def test(args: String*) = args.toList
}"""
  }

  @Test
  def testStar() = {
    val tree = treeFrom("""
    object Functions {
      "abcde".toList match {
        case Seq(car, _*) => car
      }
    }
    """)

    tree prettyPrintsTo """object Functions {
  ("abcde").toList match {
    case Seq(car, _*) => car
  }
}"""
  }

  @Test
  def testSuper() = {
    val tree = treeFrom("""
    trait Root {
      def x = "Root"
    }
    class A extends Root {
      def superA = super.x
    }
    class B extends A with Root {
      class Inner {
        val myX = B.super.x
      }
      def fromA = super[A].x
      def fromRoot = super[Root].x
    }
    """)

    tree prettyPrintsTo """trait Root {
  def x = "Root"
}

class A extends Root {
  def superA = super.x
}

class B extends A with Root {
  class Inner {
    val myX = B.super.x
  }

  def fromA = super[A].x

  def fromRoot = super[Root].x
}"""
  }
  @Test
  def testThis() = {
    val tree = treeFrom("""
    class Root {
      class Inner {
        val outer = Root.this
      }
      val self = this
    }
    """)

    tree prettyPrintsTo """class Root {
  class Inner {
    val outer = Root.this
  }

  val self = this
}"""
  }

  @Test
  def testUnapply() = {
    val tree = treeFrom("""
    object Extractor {
      def unapply(i: Int) = Some(i)
    }
    object User {
      5 match { case Extractor(i) => i }
      5 match { case a @ Extractor(i) => i }
      5 match { case a @ Extractor(i: Int) => i }
    }
    """)

    tree prettyPrintsTo """object Extractor {
  def unapply(i: Int) = Some.apply(i)
}

object User {
  5 match {
    case Extractor(i) => i
  }

  5 match {
    case a @ Extractor(i) => i
  }

  5 match {
    case a @ Extractor(i: Int) => i
  }
}"""
  }

  @Test
  def testAlteredPattern = global.ask { () =>
    val tree = treeFrom("""
    object Demo {
      5 match { case i => () }
    }
    """)

    val alterPattern = topdown {
      matchingChildren {
        transform {
          case b: Bind => Literal(Constant(5)) replaces b
        }
      }
    }

    alterPattern(tree).get prettyPrintsTo """object Demo {
  5 match {
    case 5 => ()
  }
}"""
  }

  @Test
  def testPackages() = {
    val tree = treeFrom("""
    package a
    package b.c
    package d {
      package e.f {
      }
    }
    object A
    """)

    // XXX this is wrong
    tree prettyPrintsTo """package a

package b.c

package d

package e.f

object A"""
  }

  @Test
  def testIfs() = {
    val tree = treeFrom("""
    object Functions {

      val x = if(true) false else true

      val y = if(true == false)
          true
        else if (true == true)
          false
        else
          true

      val z = if(true == false) {
          true
        } else if (true == true) {
          false
        } else {
          println("hello!")
          true
        }
    }""")

    tree prettyPrintsTo """object Functions {
  val x = if (true) false else true

  val y = if (true == false) true else if (true == true) false else true

  val z = if (true == false) true else if (true == true) false else {
    println("hello!")
    true
  }
}"""
  }

  @Test
  def testFunctions() = {

    val tree = treeFrom("""
    object Functions {
      List(1, 2) map ((i: Int) => i + 1)
      val sum: Seq[Int] => Int = _ reduceLeft (_+_)
      List(1, 2) map (_ + 1)
      List(1, 2) map (i => i + 1)
    }""")

    tree prettyPrintsTo """object Functions {
  List(1, 2).map((i: Int) => i + 1)

  val sum: Seq[Int] => Int = _.reduceLeft(_ + _)

  List(1, 2).map(_ + 1)

  List(1, 2).map(i => i + 1)
}"""
  }

  @Test
  @ScalaVersion(matches="2.10")
  def testTypeDefs() = {

    val tree = treeFrom("""
    trait Types {
      type A = Int
      type B >: Nothing <: AnyRef
      def id[C](c: C) = c
      protected type C >: Nothing
      type D <: AnyRef
    }""")

    tree prettyPrintsTo """trait Types {
  type A = Int

  type B >: Nothing <: AnyRef

  def id[C](c: C) = c

  protected type C >: Nothing ▒

  type D <: AnyRef
}"""
  }

  @Test
  @ScalaVersion(matches="2.11")
  def testTypeDefs_211() = {

    val tree = treeFrom("""
    trait Types {
      type A = Int
      type B >: Nothing <: AnyRef
      def id[C](c: C) = c
      protected type C >: Nothing
      type D <: AnyRef
    }""")

    tree prettyPrintsTo """trait Types {
  type A = Int

  type B >: Nothing <: AnyRef

  def id[C](c: C) = c

  protected type C >: Nothing <: Any

  type D >: Nothing <: AnyRef
}"""
  }

  @Test
  def testTypes() = {

    val tree = treeFrom("""
    object Rename1 {
      case class Person(name: String)
      def printName(ppp: Person) = println(ppp.name)
      def main(args: Array[String]) {
        val people: List[Person] = List(Person("Mirko"), Person("Christina"))
        people foreach printName
      }
    }""")

    tree prettyPrintsTo """object Rename1 {
  case class Person(name: String)

  def printName(ppp: Rename1.Person) = println(ppp.name)

  def main(args: Array[String]) = {
    val people: List[Rename1.Person] = List(Person.apply("Mirko"), Person.apply("Christina"))
    people.foreach({
      (ppp: Rename1.Person) => printName(ppp)
    })
  }
}"""
  }

  @Test
  def testObjectTemplate() = {

    val tree = treeFrom("""
    object Obj extends java.lang.Object {
      val self = this
    }
    """)

    tree prettyPrintsTo """object Obj extends java.lang.Object {
  val self = this
}"""
  }

  @Ignore
  @Test
  def testValOrDefDefModifiers() = {

    val tree = treeFrom("""
    class A {
      /*a*/private/*b*/def/*c*/test() = 5
      lazy val i = 5
      final protected def a() = i
    }
    """)

    tree prettyPrintsTo """class A {
  private def test() = 5

  lazy val i = 5

  final protected def a() = i
}"""
  }

  @Test
  def testClassModifiers() = {

    val tree = treeFrom("""
    package xy
    abstract class Aaa
    sealed class Bbbb
    final class Cccc
    protected sealed class Dddd
    """)

    val modTree = global.ask { () =>
      (topdown(changeSomeModifiers)).apply(tree).get
    }

    tree prettyPrintsTo """package xy

abstract class Aaa

sealed class Bbbb

final class Cccc

protected sealed class Dddd"""

    modTree prettyPrintsTo """package xy

class Aaa

class Bbbb

class Cccc

class Dddd"""
  }

  @Test
  def testSelfTypes() = {

    val tree = treeFrom("""
    trait ATrait {
      self =>
    }
    trait BTrait {
      self: ATrait =>
    }
    trait CTrait {
      self: BTrait with ATrait =>
    }
    """)

    tree prettyPrintsTo """trait ATrait {
  self =>
}

trait BTrait {
  self: ATrait =>
}

trait CTrait {
  self: BTrait with ATrait =>
}"""
  }

  @Test
  def testClassTemplates() = {

    val tree = treeFrom("""
    trait ATrait
    class ASuperClass(x: Int, val d: String)
    class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
      self_type_annotation =>
      def someMethod() {
      }
    }
    """)

    tree prettyPrintsTo """trait ATrait

class ASuperClass(x: Int, val d: String)

class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
  self_type_annotation =>
  def someMethod() = ()
}"""
  }

  @Test
  def testSuperClass() = {

    val tree = treeFrom("""
    class ASuperClass(x: Int, val d: String)
    class AClass(i: Int, var b: String) extends ASuperClass(i, b) {
    }
    """)

    tree prettyPrintsTo """class ASuperClass(x: Int, val d: String)

class AClass(i: Int, var b: String) extends ASuperClass(i, b)"""
  }

  @Test
  def testTry() = {

    val tree = treeFrom("""
    import java.io._

    object Aua {
      var file: PrintStream = null
      try {
        val out = new FileOutputStream("myfile.txt")
        file = new PrintStream(out)
      } catch {
        case ioe: IOException => println("ioe")
        case e: Exception => println("e")
      } finally {
        println("finally!")
      }
      try {
        file = new PrintStream(new FileOutputStream("myfile.txt"))
      } catch {
        case e: Exception => println("e")
      }
      try {
        file = new PrintStream(new FileOutputStream("myfile.txt"))
      } finally {
        println("finally!")
        println("finally!")
        println("finally!")
      }
    }
    """)

    tree prettyPrintsTo """import java.io._

object Aua {
  var file: java.io.PrintStream = null

  try {
    val out = new FileOutputStream("myfile.txt")
    file = new PrintStream(out)
  } catch {
    case ioe: java.io.IOException => println("ioe")
    case e: Exception => println("e")
  } finally {
    println("finally!")
  }

  try {
    file = new PrintStream(new FileOutputStream("myfile.txt"))
  } catch {
    case e: Exception => println("e")
  }

  try {
    file = new PrintStream(new FileOutputStream("myfile.txt"))
  } finally {
    println("finally!")
    println("finally!")
    println("finally!")
  }
}"""
  }

  @Ignore
  @Test
  def testEarlyDef() = {
    val tree = treeFrom("""
    trait Greeting {
      val name: String
      val msg = "How are you, "+name
    }
    class C(i: Int) extends {
      val name = "Bob"
    } with Greeting {
      println(msg)
    }
""")

    tree prettyPrintsTo """trait Greeting {
  val name: String

  val msg = "How are you, ".+(name)
}

class C(i: Int) extends {
  val name = "Bob"
} with Greeting {
  println(msg)
}"""
  }

  @Test
  def testImports() = {
    val tree = treeFrom("""
    import java.lang.{String => S}
    import java.lang.Object
    import java.lang.{String => S, Object => _, _}
    import scala.collection.mutable._
    """)

    tree prettyPrintsTo """import java.lang.{String => S}

import java.lang.Object

import java.lang.{String => S, Object => _, _}

import scala.collection.mutable._"""
  }

  @Test
  def testVals() = {
    val tree = treeFrom("""
    /*a*/package /*b*/xyz/*c*/ {
      // now a class
      trait A
      {
        val a: Int = 5 //a
        val b = "huhu" //b
      }
      trait B
      {
        val a: Int
      }
    }
    """)

    tree prettyPrintsTo """package xyz

trait A {
  val a: Int = 5

  val b = "huhu"
}

trait B {
  val a: Int
}"""
  }

  @Test
  def testMethodSignatures() = treeFrom("""
    package xy

    class A {
      def a(): Int
      def b: Int = 5
      def c() = 5
      def d = {
        val a = 5
        a
      }
      def e(i: Int) = i
      def f(i: Int)(j: Int) = i+j
      def g(i: Int, j: Int) = i+j
      def h(i: Int, j: Int): (Int, Int) = (i, j)
      def id[A](a: A) = a
    }
    """) prettyPrintsTo """package xy

class A {
  def a(): Int

  def b: Int = 5

  def c() = 5

  def d = {
    val a = 5
    a
  }

  def e(i: Int) = i

  def f(i: Int)(j: Int) = i + j

  def g(i: Int, j: Int) = i + j

  def h(i: Int, j: Int): (Int, Int) = (i, j)

  def id[A](a: A) = a
}"""

  @Test
  def testFunctionArg = treeFrom("""
    class A {
      def fun[A, B, C](fu: (A, B, C) => A): A
    }
    """) prettyPrintsTo """class A {
  def fun[A, B, C](fu: (A, B, C) => A): A
}"""

  @Test
  def partialFunctionArg = treeFrom("""
    class A {
      def main[A, B](e: Either[A, B]) {
        e match {
          case Right(_) => ()
        }
      }
    }
    """) prettyPrintsTo """class A {
  def main[A, B](e: Either[A,B]) = e match {
    case Right(_) => ()
  }
}"""

  @Test
  def operatorPrecedences1 = treeFrom("""
    class A {
      5 * (2 + 1)
    }
    """) prettyPrintsTo """class A {
  5 * (2 + 1)
}"""

  @Test
  def operatorPrecedences2 = treeFrom("""
    class A {
      5 * 2 + 1
    }
    """) prettyPrintsTo """class A {
  5 * 2 + 1
}"""

  @Test
  def operatorPrecedences3 = treeFrom("""
    class A {
      1 + 2 + 3
    }
    """) prettyPrintsTo """class A {
  1 + 2 + 3
}"""

  @Test
  def operatorPrecedences4 = treeFrom("""
    class A {
      1 + (2 + 3)
    }
    """) prettyPrintsTo """class A {
  1 + (2 + 3)
}"""
}


