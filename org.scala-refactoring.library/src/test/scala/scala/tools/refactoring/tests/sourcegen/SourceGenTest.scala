/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.sourcegen

import tests.util.TestHelper
import org.junit.Assert
import org.junit.Assert._
import sourcegen.SourceGenerator
import common.{SilentTracing, ConsoleTracing}
import tools.nsc.symtab.Flags
import tools.nsc.ast.parser.Tokens

class SourceGenTest extends TestHelper with SourceGenerator with SilentTracing {
  
  import global._
  
  override def treeForFile(file: AbstractFile) = {
    unitOfFile get file map (_.body) flatMap removeAuxiliaryTrees
  }
    
  def generateText(t: Tree): String = generate(t).asText
  
  val reverseBody = transform {
    case t: Template => t.copy(body = t.body.reverse) setPos t.pos
  }
  
  val doubleAllDefNames = transform {
    case t: DefDef => t.copy(name = t.name.toString + t.name.toString) setPos t.pos
  }
  
  val negateAllBools = transform {
    case l @ Literal(Constant(true )) => Literal(Constant(false)) setPos l.pos
    case l @ Literal(Constant(false)) => Literal(Constant(true )) setPos l.pos
  }
  
  val wrapDefRhsInBlock = transform {
    case t @ DefDef(_, _, _, _, _, _: Block) => t
    case t @ DefDef(_, _, _, _, _, rhs) => t copy (rhs = new Block(rhs :: Nil, rhs)) setPos t.pos
  }
  
  val changeSomeModifiers = transform {
    case t: ClassDef =>
      t.copy(mods = NoMods) setPos t.pos
    case t: DefDef   =>
      t.copy(mods = NoMods withPosition (Flags.PROTECTED, NoPosition) withPosition (Flags.METHOD, NoPosition)) setPos t.pos
    case t: ValDef   =>
      t.copy(mods = NoMods withPosition (Tokens.VAL,   NoPosition)) setPos t.pos
    case t => t
  }
  
  @Test
  def testSimpleIndentation() = {

    val tree = treeFrom("""
    object Functions {
        def something = 42
    }
    """)
        
    assertEquals("""
    object Functions {
        def something = {
          42
          42
        }
    }
    """, generateText(removeAuxiliaryTrees &> ↓(matchingChildren(wrapDefRhsInBlock)) apply tree get))
  }
  
  @Test
  def testIndentationOfNestedBlocks() = {
    
    val nestDefs = transform {
      case t @ DefDef(_, _, _, _, _, rhs @ Block(stats, expr)) => t copy (rhs = new Block(t :: stats, expr) setPos rhs.pos) setPos t.pos
    }

    val tree = treeFrom("""
    object Functions {
      def something = {
        println("huhu") 
        42
      }
    }
    """)
    
    assertEquals("""
    object Functions {
      def something = {
        def something = {
          println("huhu") 
          42
        }
        println("huhu") 
        42
      }
    }
    """, generateText(removeAuxiliaryTrees &> ↑(matchingChildren(nestDefs)) apply tree get))
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
        
    assertEquals("""
    object Functions {
      val a = new String("hello")
      def createNew = new {
        println("hello from an anonymous class")
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
    class Throw1 {
      throw new Exception("hu!")
    }

    class Throw2 {
      var msg = "   "
      val e = new Exception(msg) 
      throw e
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
    import scala.reflect.BeanProperty
    class ATest {
      @BeanProperty
      var status = ""
    }

    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def allNeededParenthesesArePrinted() = {
    
    val tree = treeFrom("""
    class Test {
      val x = true && !(true && false)
    }
    """)
        
    assertEquals("""
    class Test {
      val x = true && !(true && false)
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def literalIdentifier() = {
    
    val tree = treeFrom("""
    class Test {
      val `class` = 5
      println(`class`)
      val threadyield = Thread.`yield` _
    }
    """)
        
    assertEquals("""
    class Test {
      val `class` = 5
      println(`class`)
      val threadyield = Thread.`yield` _
    }
    """, generateText(removeAuxiliaryTrees apply tree get))     
  }
  
  @Test
  def multipleAssignmentWithAnnotatedTree() = {
    
    val tree = treeFrom("""
    class Test {
      val (a, b) = 1 → 2
      val (c, d, e, f) = (1, 2, 3, 4)
      val (g, h, i) = inMethod()

      def inMethod() = {
        println("in method")
        val (a, b, c) = (1, 2, 3)
        println("in method")
        (a, b, c)
      }
    }
    """)
        
    assertEquals("""
    class Test {
      val (a, b) = 1 → 2
      val (c, d, e, f) = (1, 2, 3, 4)
      val (g, h, i) = inMethod()

      def inMethod() = {
        println("in method")
        val (a, b, c) = (1, 2, 3)
        println("in method")
        (a, b, c)
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))     
  }
  
  @Test
  def testExistential() = {
    
    val tree = treeFrom("""
    class A(l: List[_])

    class B(l: List[T] forSome { type T })
    """)
        
    assertEquals("""
    class A(l: List[_])

    class B(l: List[T] forSome { type T })
    """, generateText(removeAuxiliaryTrees apply tree get))
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
    
    object Test4 {
       def doNothing {
       }
    }
    class Test4 {
       def bar() {
         Transaction.run[Unit](Transaction.Kind.ReadOnly)
       }
    }""")
        
    assertEquals("""
    package com.somedomain.test

    object Transaction {
      object Kind {
        val ReadOnly = true
      }
      def run[T](readOnyl: Boolean) = ()
    }
    
    object Test4 {
       def doNothing {
       }
    }
    class Test4 {
       def bar() {
         Transaction.run[Unit](Transaction.Kind.ReadOnly)
       }
    }""", generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
    trait A
    trait B
    abstract class C(val a: A with B) {
      def method(x: A with B with C {val x: Int}): A with B
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testSingletonTypeTree() = {
    
    val tree = treeFrom("""
    trait A {
      def doSomething(): this.type
    }
    """)
        
    assertEquals("""
    trait A {
      def doSomething(): this.type
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testSelectFromTypeTree() = {
    
    val tree = treeFrom("""
    trait A {
      type T
    }

    class B(t: A#T)
    """)
        
    assertEquals("""
    trait A {
      type T
    }

    class B(t: A#T)
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
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
        
    assertEquals("""
    package common {
      trait Tracing
      trait PimpedTrees
    }

    trait AbstractPrinter {
      this: common.Tracing with common.PimpedTrees =>
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
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
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
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
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testPlusEquals() = {
    val tree = treeFrom("""
      trait Demo2 {
        var assignee = 1
        assignee += -42
      }""")
      
    assertEquals("""
      trait Demo2 {
        var assignee = 1
        assignee += -42
      }""", generateText(tree))
    
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
      
    assertEquals("""
      trait Demo1 {
        def method {
          var i = 0
          i = 1
        }
      }""", generateText(tree))
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
      
    assertEquals("""
      package oneFromMany
      class Demo(val a: String,  /*(*/private var _i: Int/*)*/  ) {
        def i_=(i: Int) = {
          _i = i
        }
      }""", generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def typeParametersAreSeparatedByComma() = {
    val tree = treeFrom("""
      class MethodWithTypeParam {
        def foo[A, B, C] = 1
      }""")
      
    assertEquals("""
      class MethodWithTypeParam {
        def foo[A, B, C] = 1
      }""", generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testClassConstructorParams() = {
    val tree = treeFrom("""
      class Demo1(a: String, b: Int)
      class Demo2(a: String, b: Int)
    """)
      
    assertEquals("""
      class Demo1(a: String, b: Int)
      class Demo2(a: String, b: Int)
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testMatches() = {
    val tree = treeFrom("""
    object Functions {
      List(1,2) match {
        case i => i
      }

      List(1,2) collect {
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
        
    assertEquals("""
    object Functions {
      List(1,2) match {
        case i => i
      }

      List(1,2) collect {
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
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testReturn() = {
    val tree = treeFrom("""
    object Functions {
      def test {
        return 5
      }
    }
    """)
        
    assertEquals("""
    object Functions {
      def test {
        return 5
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testVarArgs() = {
    val tree = treeFrom("""
    object Functions {
      def test(args: String*) = args.toList
    }
    """)
        
    assertEquals("""
    object Functions {
      def test(args: String*) = args.toList
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
    object Functions {
      "abcde".toList match {
        case Seq(car, _*) => car 
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
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
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
    class Root {
      class Inner {
        val outer = Root.this
      }
      val self = this
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
    object Extractor {
      def unapply(i: Int) = Some(i)
    }
    object User {
      5 match { case Extractor(i) => i }
      5 match { case a @ Extractor(i) => i }
      5 match { case a @ Extractor(i: Int) => i }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
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
        
    assertEquals("""
    package a
    package b.c
    package d {
      package e.f {
      }
    }
    object A
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testIf() = {
    
    val tree = treeFrom("""
    object Functions {
      val y = if(true)
          true
        else if (true)
          false
        else
          true
    }""")
    
    assertEquals("""
    object Functions {
      val y = if(false)
          false
        else if (false)
          true
        else
          false
    }""", generateText(removeAuxiliaryTrees &> ↓(matchingChildren(negateAllBools)) apply tree get))     
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
    
    assertEquals("""
    object Functions {

      val x = if(false) true else false

      val y = if(false == true)
          false
        else if (false == false)
          true
        else
          false

      val z = if(false == true) {
          false
        } else if (false == false) {
          true
        } else {
          println("hello!")
          false
        }
    }""", generateText(removeAuxiliaryTrees &> ↓(matchingChildren(negateAllBools)) apply tree get))
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
      
    assertEquals("""
    object Functions {
      List(1, 2) map ((i: Int) => i + 1)
      val sum: Seq[Int] => Int = _ reduceLeft (_+_)
      List(1, 2) map (_ + 1)
      List(1, 2) map (i => i + 1)
    }""", generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testTypeDefs() = {
    
    val tree = treeFrom("""
    trait Types {
      type A = Int
      type B >: Nothing <: AnyRef
      def id[C](c: C) = c
      protected type C >: Nothing
      type D <: AnyRef
    }""")
    
    assertEquals("""
    trait Types {
      type A = Int
      type B >: Nothing <: AnyRef
      def id[C](c: C) = c
      protected type C >: Nothing
      type D <: AnyRef
    }""", generateText(removeAuxiliaryTrees apply tree get))
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
    
    assertEquals("""
    object Rename1 {
      case class Person(name: String)
      def printName(ppp: Person) = println(ppp.name)
      def main(args: Array[String]) {
        val people: List[Person] = List(Person("Mirko"), Person("Christina"))
        people foreach printName
      }
    }""", generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testObjectTemplate() = {
    
    val tree = treeFrom("""
    object Obj extends java.lang.Object {
      val self = this
    }
    """)
    
    assertEquals("""
    object Obj extends java.lang.Object {
      val self = this
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testValOrDefDefModifiers() = {
    
    val tree = treeFrom("""
    class A {
      /*a*/private/*b*/def/*c*/test() = 5
      lazy val i = 5
      final protected def a() = i
    }
    """)
    
    assertEquals("""
    class A {
      /*a*/protected def test() = 5
      val i = 5
      protected def a() = i
    }
    """, generateText((removeAuxiliaryTrees &> ↓(changeSomeModifiers)) apply tree get))
  }
  
  @Test
  def testClassModifiers() = {
    
    val tree = treeFrom("""
    package xy
    abstract class A
    sealed class B
    final class C
    protected sealed class D
    """)
    
    val modTree = (removeAuxiliaryTrees &> ↓(changeSomeModifiers)) apply tree get
    
    assertEquals("""
    package xy
    class A
    class B
    class C
    class D
    """, generateText(modTree))
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
    
    assertEquals("""
    trait ATrait {
      self =>
    }
    trait BTrait {
      self: ATrait =>
    }
    trait CTrait {
      self: BTrait with ATrait =>
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
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
    
    assertEquals("""
    trait ATrait
    class ASuperClass(x: Int, val d: String)
    class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
      self_type_annotation =>
      def someMethod() {
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testSuperClass() = {
    
    val tree = treeFrom("""
    class ASuperClass(x: Int, val d: String)
    class AClass(i: Int, var b: String) extends ASuperClass(i, b) {
    }
    """)
    
    assertEquals("""
    class ASuperClass(x: Int, val d: String)
    class AClass(i: Int, var b: String) extends ASuperClass(i, b) {
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testTry() = {
    
    val tree = treeFrom("""
    import java.io._
    object A {
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
    
    assertEquals("""
    import java.io._
    object A {
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
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
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

    assertEquals("""
    trait Greeting {
      val name: String
      val msg = "How are you, "+name
    }
    class C(i: Int) extends {
      val name = "Bob"
    } with Greeting {
      println(msg)
    }
""", generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testImports() = {
    val tree = treeFrom("""
    import java.lang.{String => S}
    import java.lang.Object
    import java.lang.{String => S, Object => _, _}
    import scala.collection.mutable._
    """)

    assertEquals("""
    import java.lang.{String => S}
    import java.lang.Object
    import java.lang.{String => S, Object => _, _}
    import scala.collection.mutable._
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def testMethods() = {
    
    val tree = treeFrom("""
      trait ATest
      {
        def abcd[T](a: String, b: Int): Int
        def a() = 5
        def b = 42
        def ab(i: Int)(j: Int) = (i ,     j)
        def timesTwo(i: Int) = {
          i.*(5)
          i * 2
        }
        def square {
          def nested(i: Int) = {
            i * i
          }
          nested(5)
        }
      }
    """)

    assertEquals("""
      trait ATest
      {
        def squaresquare {
          def nestednested(i: Int) = {
            i * i
          }
          nested(5)
        }
        def timesTwotimesTwo(i: Int) = {
          i.*(5)
          i * 2
        }
        def abab(i: Int)(j: Int) = (i ,     j)
        def bb = 42
        def aa() = 5
        def abcdabcd[T](a: String, b: Int): Int
      }
    """, generateText(removeAuxiliaryTrees &> ↓(matchingChildren(doubleAllDefNames)) &> ↓(matchingChildren(reverseBody)) apply tree get)) 
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

    assertEquals("""
    /*a*/package /*b*/xyz/*c*/ {
      // now a class
      trait A
      {
        val b = "huhu" //b
        val a: Int = 5 //a
      }
      trait B
      {
        val a: Int
      }
    }
    """, generateText(removeAuxiliaryTrees &> ↓(matchingChildren(reverseBody)) apply tree get))
  }
}

