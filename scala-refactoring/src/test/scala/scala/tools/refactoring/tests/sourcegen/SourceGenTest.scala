/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests.sourcegen

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert
import org.junit.Assert._
import scala.tools.refactoring.sourcegen._
import scala.tools.refactoring.common._
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.io.AbstractFile
import tools.nsc.symtab.Flags
import scala.tools.nsc.ast.parser.Tokens

@Test
class SourceGenTest extends TestHelper with SourceGen with ConsoleTracing {
  
  import global._
  
  override def treeForFile(file: AbstractFile) = {
    unitOfFile get file map (_.body) flatMap removeAuxiliaryTrees
  }
  
  implicit def treeToPrettyPrint(original: Tree) = new {
    def cleanTree(t: Tree) = (removeAuxiliaryTrees &> emptyAllPositions)(t).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generate(cleanTree(original)).asText)
  }
  
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
    """, generate(removeAuxiliaryTrees &> ↓(matchingChildren(wrapDefRhsInBlock)) apply tree get).asText)
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
    """, generate(removeAuxiliaryTrees &> ↑(matchingChildren(nestDefs)) apply tree get).asText)
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    // terrible, but don't know how to do better :/
    tree prettyPrintsTo """object Functions {
  val a = new String("hello")
  def createNew = {
    class $anon {
      println("hello from an anonymous class")
    }
    new 
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
        
    assertEquals("""
    class Throw1 {
      throw new Exception("hu!")
    }

    class Throw2 {
      var msg = "   "
      val e = new Exception(msg) 
      throw e
    }
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
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
        
    assertEquals("""
    import scala.reflect.BeanProperty
    class ATest {
      @BeanProperty
      var status = ""
    }

    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
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
        
    assertEquals("""
    class Test {
      val x = true && !(true && false)
    }
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    tree prettyPrintsTo """class Test {
  val x = true.&&(true.&&(false).!)
}"""
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    //XXX tree prettyPrintsTo """"""
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    tree prettyPrintsTo """class A(l: List[_])
class B(l: List[T] forSome {type T})"""
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
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
        
    assertEquals("""
    trait A {
      def doSomething(): this.type
    }
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    tree prettyPrintsTo """trait A {
  def doSomething: this.type
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
        
    assertEquals("""
    trait A {
      type T
    }

    class B(t: A#T)
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    tree prettyPrintsTo """trait A {
  type T
}
class B(t: A#T)"""
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    tree prettyPrintsTo """trait WhileLoop {
  while(true.!=(false)){
    println("The world is still ok!")
  }
  while(true.!=(false)){
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    tree prettyPrintsTo """trait WhileLoop {
  do println("The world is still ok!") while(true)
  do println("The world is still ok!") while(true.!=(false))
  do {
    println("The world is still ok!")
    println("The world is still ok!")
  } while(true)
}"""
  }
  
  @Test
  def testPlusEquals() = {
    val tree = treeFrom("""
      trait Demo {
        var assignee = 1
        assignee += -42
      }""")
      
    assertEquals("""
      trait Demo {
        var assignee = 1
        assignee += -42
      }""", generate(tree).asText)
    
    //TODO fixme tree prettyPrintsTo """"""
  }
  
  @Test
  def testAssign() = {
    val tree = treeFrom("""
      trait Demo {
        def method {
          var i = 0
          i = 1
        }
      }""")
      
    assertEquals("""
      trait Demo {
        def method {
          var i = 0
          i = 1
        }
      }""", generate(tree).asText)
    
    tree prettyPrintsTo """trait Demo {
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
      
    assertEquals("""
      package oneFromMany
      class Demo(val a: String,  /*(*/private var _i: Int/*)*/  ) {
        def i_=(i: Int) = {
          _i = i
        }
      }""", generate(removeAuxiliaryTrees apply tree get).asText)
    
    tree prettyPrintsTo """package oneFromMany
class Demo(val a: String, private var _i: Int) {
  def i_=(i: Int) = _i = i
}"""
  }
  
  @Test
  def testClassConstructorParams() = {
    val tree = treeFrom("""
      class Demo1(a: String, b: Int)
      class Demo2(a: String, b: Int)""")
      
    assertEquals("""
      class Demo1(a: String, b: Int)
      class Demo2(a: String, b: Int)""", generate(removeAuxiliaryTrees apply tree get).asText)
    
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    // FIXME problem with ::
    tree prettyPrintsTo """object Functions {
  List(1, 2) match {
    case i => i
  }
  List(1, 2).collect {
    case i if i.>(5) => i
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    tree prettyPrintsTo """object Functions {
  def test = return {
    5
  }
}"""
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
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
        
    assertEquals("""
    object Functions {
      "abcde".toList match {
        case Seq(car, _*) => car 
      }
    }
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
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
        
    assertEquals("""
    class Root {
      class Inner {
        val outer = Root.this
      }
      val self = this
    }
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
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
        
    assertEquals("""
    object Extractor {
      def unapply(i: Int) = Some(i)
    }
    object User {
      5 match { case Extractor(i) => i }
      5 match { case a @ Extractor(i) => i }
      5 match { case a @ Extractor(i: Int) => i }
    }
    """, generate(removeAuxiliaryTrees apply tree get).asText)     
    
    tree prettyPrintsTo """object Extractor {
  def unapply(i: Int) = Some(i)
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)
    
    // XXX this is wrong
    tree prettyPrintsTo """package a
package b.c
package d
package e.f
object A"""
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
    }""", generate(removeAuxiliaryTrees &> ↓(matchingChildren(negateAllBools)) apply tree get).asText)     
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
    }""", generate(removeAuxiliaryTrees &> ↓(matchingChildren(negateAllBools)) apply tree get).asText)     
    
    tree prettyPrintsTo """object Functions {
  val x = if (true) false else true
  val y = if (true.==(false)) true else if (true.==(true)) false else true
  val z = if (true.==(false)) true else if (true.==(true)) false else {
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
      
    assertEquals("""
    object Functions {
      List(1, 2) map ((i: Int) => i + 1)
      val sum: Seq[Int] => Int = _ reduceLeft (_+_)
      List(1, 2) map (_ + 1)
      List(1, 2) map (i => i + 1)
    }""", generate(removeAuxiliaryTrees apply tree get).asText)     
      
    tree prettyPrintsTo """object Functions {
  List(1, 2).map((i: Int) => i.+(1))
  val sum: (Seq[Int]) => Int = _.reduceLeft(_.+(_))
  List(1, 2).map(_.+(1))
  List(1, 2).map((i) => i.+(1))
}"""
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
    }""", generate(removeAuxiliaryTrees apply tree get).asText)
    
    tree prettyPrintsTo """trait Types {
  type A = Int
  type B >: Nothing <: AnyRef
  def id[C](c: C) = c
  protected type C >: Nothing 
  type D <: AnyRef
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
    
    assertEquals("""
    object Rename1 {
      case class Person(name: String)
      def printName(ppp: Person) = println(ppp.name)
      def main(args: Array[String]) {
        val people: List[Person] = List(Person("Mirko"), Person("Christina"))
        people foreach printName
      }
    }""", generate(removeAuxiliaryTrees apply tree get).asText)
    
    tree prettyPrintsTo """object Rename1 {
  case class Person(name: String)
  def printName(ppp: Rename1.Person) = println(ppp.name)
  def main(args: Array[String]) = {
    val people: List[Rename1.Person] = List(Person("Mirko"), Person("Christina"))
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
    
    assertEquals("""
    object Obj extends java.lang.Object {
      val self = this
    }
    """, generate(removeAuxiliaryTrees apply tree get).asText)
    
    tree prettyPrintsTo """object Obj extends java.lang.Object {
  val self = this
}"""
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
    """, generate((removeAuxiliaryTrees &> ↓(changeSomeModifiers)) apply tree get).asText)
    
    tree prettyPrintsTo """class A {
  private def test = 5
  lazy val i = 5
  final protected def a = i
}"""
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
    """, generate(modTree).asText)
    
    tree prettyPrintsTo """package xy
abstract class A
sealed class B
final class C
protected sealed class D"""
    
    modTree prettyPrintsTo """package xy
class A
class B
class C
class D"""
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)
    
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
    
    assertEquals("""
    trait ATrait
    class ASuperClass(x: Int, val d: String)
    class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
      self_type_annotation =>
      def someMethod() {
      }
    }
    """, generate(removeAuxiliaryTrees apply tree get).asText)
    
    tree prettyPrintsTo """trait ATrait
class ASuperClass(x: Int, val d: String)
class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
  self_type_annotation =>
  def someMethod {
  }
}"""
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
    """, generate(removeAuxiliaryTrees apply tree get).asText)
    
    tree prettyPrintsTo """import java.io._
object A {
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
""", generate(removeAuxiliaryTrees apply tree get).asText)
    
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

    assertEquals("""
    import java.lang.{String => S}
    import java.lang.Object
    import java.lang.{String => S, Object => _, _}
    import scala.collection.mutable._
    """, generate(removeAuxiliaryTrees apply tree get).asText)
    
    tree prettyPrintsTo """import java.lang.{String => S}
import java.lang.Object
import java.lang.{String => S, Object => _, _}
import scala.collection.mutable._"""
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
    """, generate(removeAuxiliaryTrees &> ↓(matchingChildren(doubleAllDefNames)) &> ↓(matchingChildren(reverseBody)) apply tree get).asText) 
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
    """, generate(removeAuxiliaryTrees &> ↓(matchingChildren(reverseBody)) apply tree get).asText)
    
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
      def b(): Int = 5
      def c() = 5
      def d() = {
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
  def a: Int
  def b: Int = 5
  def c = 5
  def d = {
    val a = 5
    a
  }
  def e(i: Int) = i
  def f(i: Int)(j: Int) = i.+(j)
  def g(i: Int, j: Int) = i.+(j)
  def h(i: Int, j: Int): (Int, Int) = (i, j)
  def id[A](a: A) = a
}"""
}

