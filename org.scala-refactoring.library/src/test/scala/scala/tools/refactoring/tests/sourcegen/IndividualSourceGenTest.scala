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
import common.Change
import tools.nsc.symtab.Flags
import tools.nsc.ast.parser.Tokens

class IndividualSourceGenTest extends TestHelper with SourceGenerator with SilentTracing with transformation.TreeFactory {
  
  import global._
  
  def generateText(t: Tree): String = global.ask { () =>
    createText(t, sourceFile = Some(t.pos.source))
  }
  
  implicit def treeToPrettyPrint(original: Tree) = new {
    def cleanTree(t: Tree) = (removeAuxiliaryTrees &> emptyAllPositions)(t).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generateText(cleanTree(original)))
  }
  
  def getFirstMethod(source: String) = treeFrom(source) match {
    case PackageDef(_, ClassDef(_, _, _, Template(_, _, (d: DefDef) :: _)) :: _) => d
    case _ => Assert.fail(); throw new Exception("unreachable")
  }
  
  def testDefDefWithNoPositionAndOriginalPosition(src: String, exp1: String, exp2: String) {
    
    val originalDefDef = getFirstMethod(src)
    
    val newDefDef = originalDefDef.copy()
        
    assertEquals(exp1, generate(newDefDef, sourceFile = Some(originalDefDef.pos.source)).asText)

    newDefDef.setPos(originalDefDef.pos)
    
    assertEquals(exp2, generate(newDefDef, sourceFile = Some(originalDefDef.pos.source)).asText)    
  }
  
  @Test
  def testNewDefDefWithOriginalContent1() = {
    
    testDefDefWithNoPositionAndOriginalPosition(
    """
    trait A {
      def someMethod: Unit
    }
    """, 
    """def someMethod: Unit {
}""", 
    """
      def someMethod: Unit""")
  }
  
  @Test
  def testNewDefDefWithOriginalContent2() = {
    
    testDefDefWithNoPositionAndOriginalPosition(
    """
    trait A {
      def someMethod(): String
    }
    """, 
    """def someMethod(): String {
}""", 
    """
      def someMethod(): String""")
  }
  
  @Test
  def testNewDefDefWithOriginalContent3() = {
    
    testDefDefWithNoPositionAndOriginalPosition(
    """
    trait A {
      def someMethod[T](): T
    }
    """, 
    """def someMethod[T](): T {
}""", 
    """
      def someMethod[T](): T""")
  }
  
  @Test
  def testNewDefDefWithOriginalContent4() = {
    
    testDefDefWithNoPositionAndOriginalPosition(
    """
    trait A {
      protected def someMethod[T](param1: Int, param2: T)(param3: => T)
    }
    """, 
    """protected def someMethod[T](param1: Int, param2: T)(param3: => T) {
}""", 
    """
      protected def someMethod[T](param1: Int, param2: T)(param3: => T)""")
  }
  
  @Test
  def selfTypeAnnotation {
    val src = """
trait tr[A] {
  self: List[A] => 
  
  def asd() {
  }
}
"""
    val ast = treeFrom(src)    
    val defdef = mkDefDef(name = "member", body = List(Ident(newTermName("()"))))
    
    val transformedAst = topdown {
      matchingChildren {
        transform {
          case t: Template => t.copy(body = t.body ::: List(defdef)) setPos t.pos
        }
      }
    } apply ast
    
    assertEquals("""
trait tr[A] {
  self: List[A] => 
  
  def asd() {
  }
  
  def member() = {
    ()
  }
}
""", common.Change.applyChanges(refactor(transformedAst.toList), src))
  }
  
  @Test
  def addMethodToEmptyTraitWithSelfTypeAnnotation {
    val src = """
trait tr[A] {
  self: List[A] => 

}
"""
    val ast = treeFrom(src)
    
    val defdef = mkDefDef(name = "member", body = List(Ident(newTermName("()"))))
    
    val transformedAst = topdown {
      matchingChildren {
        transform {
          case t: Template => t.copy(body = t.body ::: List(defdef)) setPos t.pos
        }
      }
    } apply ast
    
    assertEquals("""
trait tr[A] {
  self: List[A] => 
  def member() = {
    ()
  }

}
""", common.Change.applyChanges(refactor(transformedAst.toList), src))
  }
  
  @Test
  def misprintedFunctionDefinition {
        
    val ast = treeFrom("""
      package generated

      object primitive {

        def member[A](a: A, li: KIVList[A]): Boolean = {
          if (li.emptyp) false
          else {
            (a equals li.car) || member(a, li.cdr)
          }
        }
      }
    """)
    
    val transformedAst = topdown {
      matchingChildren {
        transform {
          case t: DefDef => t.copy()
        }
      }
    } apply ast

    val changes = refactor(transformedAst.toList)
    assertEquals("""if (li.emptyp) false
else {
  (a equals li.car) || member(a, li.cdr)
""", changes.head.text)
  }
  
  @Test
  def valDefRhsAlone(): Unit = {
    
    val tree = treeFrom("""
    object O {
      val a = {4 + 3  }
    }
    """)
    
    val valDef = tree match {
      case PackageDef(_, ModuleDef(_, _, Template(_, _, _ :: (v: ValDef) :: _)) :: _) => v
      case _ => Assert.fail(); emptyValDef // too bad fail does not return Nothing
    }
        
    assertEquals("""val a = {4 + 3  }""", generate(removeAuxiliaryTrees apply valDef get, sourceFile = Some(tree.pos.source)).center.asText)
          
    assertEquals("""{4 + 3  }""", generate(removeAuxiliaryTrees apply valDef.rhs get, sourceFile = Some(tree.pos.source)).asText)
          
    assertEquals(0, createChanges(List(valDef)).size)
  }
  
  @Test
  @Ignore
  def newWith(): Unit = {
    
    val ast = treeFrom("""
    class A(x: Int)
    trait B
    object AWithB {
      val ab = new A(10)
    }
    """)
    
    val transformedAst = topdown {
      matchingChildren {
        transform {
          case t: ValDef if t.name.toString == "ab " => 

            val newRhs = Block(
              mkClass(
                name = "$anon", 
                parents = Ident(newTermName("A")) :: Ident(newTermName("B")) :: Nil,
                superArgs = Literal(Constant(5)) :: Nil
              ), 
              Apply(Select(New(Ident(newTermName("$anon"))), nme.CONSTRUCTOR), Nil)
            )
            
            t.copy(rhs = newRhs)
        }
      }
    } apply ast
    
    assertEquals("""
    class A(x: Int)
    trait B
    object AWithB {
      val ab = new A(5) with B
    }
    """, generateText(transformedAst.get))
  }
  
  @Test
  def modifiedDefDef(): Unit = {
    
    val originalDefDef = treeFrom("""
    object Account {
       def amethod(v: Int, a: Account): Unit = {
         a.add(v)
       }
    }
    class Account {
       // before value
       var value = 4
       // after value
    
       def add(v: Int) {
         value += v
       }
    }
    """) match {
      case PackageDef(_, ModuleDef(_, _, Template(_, _, _ :: (v: DefDef) :: _)) :: _) => v
      case _ => Assert.fail(); sys.error("") // too bad fail does not return Nothing
    }
    
    val newRHS1 = Apply(Select(Ident(newTermName("com")),newTermName("synchronized")), List(shallowDuplicate(originalDefDef.rhs) setPos NoPosition))
    
    val newDefDef1 = originalDefDef copy (rhs = newRHS1) setPos originalDefDef.pos
        
    assertEquals("""def amethod(v: Int, a: Account): Unit = com.synchronized(a.add(v))""", generate(removeAuxiliaryTrees apply newDefDef1 get, sourceFile = Some(originalDefDef.pos.source)).center.asText)
   
    assertEquals("""
       def amethod(v: Int, a: Account): Unit = com.synchronized(a.add(v))""", createFragment(removeAuxiliaryTrees apply newDefDef1 get).asText)
   
    val newRHS2 = Apply(Select(Ident(newTermName("com")), newTermName("synchronized")), List(originalDefDef.rhs))
    
    val newDefDef2 = originalDefDef copy (rhs = newRHS2) setPos originalDefDef.pos
        
    assertEquals("""def amethod(v: Int, a: Account): Unit = com.synchronized({
       a.add(v)
       })""", generate(removeAuxiliaryTrees apply newDefDef2 get, sourceFile = Some(originalDefDef.pos.source)).center.asText)
          
    assertEquals("""
       def amethod(v: Int, a: Account): Unit = com.synchronized({
       a.add(v)
       })""", createFragment(removeAuxiliaryTrees apply newDefDef2 get).asText)
 
       
    assertEquals(1, createChanges(List(newDefDef1)).size)
    
    assertEquals(1, createChanges(List(newDefDef2)).size)
  }
  
  @Test
  def modifiedDefDefWithFor(): Unit = {
    
    val originalDefDef = treeFrom("""
    object Account1 {
      def addInterest(accounts: Array[Account1], rate: Float) {
        for (a <- accounts) {
          println(a.value)
        }
      }
    }
    class Account1 {
       // before value
       var value = 4
       // after value
    
       def add(v: Int) {
         value += v
       }
    }
    """) match {
      case PackageDef(_, ModuleDef(_, _, Template(_, _, _ :: (v: DefDef) :: _)) :: _) => v
      case _ => Assert.fail(); sys.error("") // too bad fail does not return Nothing
    }
    
    val newRHS1 = new Block(List(Apply(Select(Ident(newTermName("com")),newTermName("synchronized")), List(originalDefDef.rhs))), EmptyTree)
    
    val newDefDef1 = originalDefDef copy (rhs = newRHS1) setPos originalDefDef.pos
    
    assertEquals("""
      def addInterest(accounts: Array[Account1], rate: Float) {
        com.synchronized({
        for (a <- accounts) {
          println(a.value)
        }
      })
      }""", createFragment(removeAuxiliaryTrees apply newDefDef1 get).asText)

  }
  
  @Test
  def manyParentheses(): Unit = {
    
    val tree = treeFrom("""
    class VBox[T](i: Int)
    class Test {
     var box = new VBox[Int]({ 2 + 4 })
    }
    """)
      
    assertEquals("""
    class VBox[T](i: Int)
    class Test {
     var box = new VBox[Int]({ 2 + 4 })
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def bracesAndComments(): Unit = {
    
    val tree = treeFrom("""
package com.megaannum.test

class Foo3 {
   // before idFoo
   private var idFoo = { 3 + 4 }
   // after idFoo

   // before getIdFoo
   def getIdFoo = idFoo
   // after getIdFoo
   def setIdFoo(id: Int) = idFoo = id
}
    """)
      
    assertEquals("""
package com.megaannum.test

class Foo3 {
   // before idFoo
   private var idFoo = { 3 + 4 }
   // after idFoo

   // before getIdFoo
   def getIdFoo = idFoo
   // after getIdFoo
   def setIdFoo(id: Int) = idFoo = id
}
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def bracesAndComments2(): Unit = {
    
    val tree = treeFrom("""
package com.megaannum.test

class Foo3 {
   // before idFoo
   private var idFoo = { 3 + 4 }
   // after idFoo
}
    """)
      
    assertEquals("""
package com.megaannum.test

class Foo3 {
   // before idFoo
   private var idFoo = { 3 + 4 }
   // after idFoo
}
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def missingDot(): Unit = {
    
    val tree = treeFrom("""
class Foo3 {
   var _idFoo = 7
   this._idFoo = 5
}
    """)
      
    assertEquals("""
class Foo3 {
   var _idFoo = 7
   this._idFoo = 5
}
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def doubledName(): Unit = {
    
    val tree = treeFrom("""
class Account2 {
   var value = 0
   def add1(v: Int) {
     value += v
   }
}
    """)
      
    assertEquals("""
class Account2 {
   var value = 0
   def add1(v: Int) {
     value += v
   }
}
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def extraParens(): Unit = {
    
    val tree = treeFrom("""
object Acco {
   def amethod(v: Int, a: Int) = 
     synchronized({
       v + a
     })
}
    """)
      
    assertEquals("""
object Acco {
   def amethod(v: Int, a: Int) = 
     synchronized({
       v + a
     })
}
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def namedArgs(): Unit = {
    
    val tree = treeFrom("""
    object NamedArguments {
      def p(i: Int, before: String, separator: String, after: String) = ()
 
      p(5, separator = ", ", before = "\\{", after = "\\}")  
    }
    """)
      
    assertEquals("""
    object NamedArguments {
      def p(i: Int, before: String, separator: String, after: String) = ()
 
      p(5, separator = ", ", before = "\\{", after = "\\}")  
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def namedArg(): Unit = {
    
    val tree = treeFrom("""
    object NamedArg {
      def p(first: String, second: Int) = ()
 
      def callP = {
        p(second = 42, first = "-")
      }
    }
    """)
      
    assertEquals("""
    object NamedArg {
      def p(first: String, second: Int) = ()
 
      def callP = {
        p(second = 42, first = "-")
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def namedArgWithDefault(): Unit = {
    
    val tree = treeFrom("""
    object NamedArgWithDefault {
      def p(first: String = "default", second: Int) = ()
 
      def callP = {
        p(second = 42)
      }
    }
    """)
      
    assertEquals("""
    object NamedArgWithDefault {
      def p(first: String = "default", second: Int) = ()
 
      def callP = {
        p(second = 42)
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def patternMatch(): Unit = {
    
    val tree = treeFrom("""
    object O {
      case class ModuleDef(i: Int, s: String)

      ModuleDef(42, "foo") match {
        case ModuleDef(theI, theS) =>
//        mods.annotations map traverse
          val mods_ = theS
          mods_ + theI
      }
    }
    """)
      
    assertEquals("""
    object O {
      case class ModuleDef(i: Int, s: String)

      ModuleDef(42, "foo") match {
        case ModuleDef(theI, theS) =>
//        mods.annotations map traverse
          val mods_ = theS
          mods_ + theI
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def patternMatchOnTupel(): Unit = {
    
    val tree = treeFrom("""
    object O {
      (42, "foo") match {
        case (theI, theS) =>
          ()
      }
    }
    """)
      
    assertEquals("""
    object O {
      (42, "foo") match {
        case (theI, theS) =>
          ()
      }
    }
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def newObject(): Unit = {
    
    val tree = treeFrom("""
    object Foo {
       def apply() = new Foo
    }
    
    class Foo
    """)
      
    assertEquals("""
    object Foo {
       def apply() = new Foo
    }
    
    class Foo
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def newParameterizedObject(): Unit = {
    
    val tree = treeFrom("""
object Foo4 {
   def apply[T](name: String) = new Foo4[T](name)
}

class Foo4[T](name: String) {
   def echo[T](t: T):T = t
}
    """)
      
    assertEquals("""
object Foo4 {
   def apply[T](name: String) = new Foo4[T](name)
}

class Foo4[T](name: String) {
   def echo[T](t: T):T = t
}
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def newParameterizedObject2(): Unit = {
    
    val tree = treeFrom("""
object Foo5 {
   def apply() = { new Foo5 }
}
class Foo5
    """)
      
    assertEquals("""
object Foo5 {
   def apply() = { new Foo5 }
}
class Foo5
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  } 

  @Test
  def annotation(): Unit = {
    
    val tree = treeFrom("""
import java.lang.String
import scala.annotation.StaticAnnotation
        
class RunWith(c: Class[_]) extends StaticAnnotation

/*(*/  /*)*/

@RunWith(classOf[String])
class Test
    """)
      
    assertEquals("""
import java.lang.String
import scala.annotation.StaticAnnotation
        
class RunWith(c: Class[_]) extends StaticAnnotation

/*(*/  /*)*/

@RunWith(classOf[String])
class Test
    """, generateText(removeAuxiliaryTrees apply tree get))
    
    assertEquals(0, createChanges(List(tree)).size)
  }
  
  @Test
  def testFunctionArg() {
    val ast = treeFrom(
"""
class A {
def fun[A, B, C](fu: (A, B, C) => A): A
}
""")
    assertEquals("""
class A {
def fun[A, B, C](fu: (A, B, C) => A): A
}
""", generateText(ast))
  }
  
  @Test
  def testClassParameter() {
    val ast = treeFrom("""
class A(a: Int) {
}
""")
    assertEquals("""
class A(a: Int) {
}
""", generateText(ast))
    }
  
  @Test
  def testPrintParentheses() {

    val ast = treeFrom("""
    trait tr {
      def bar(s: String) = s
      def foo() {
        bar("ASD")
      }
    }
    """)

    val result = topdown {
      matchingChildren {
        transform {
          case a: Apply =>
            a.copy(args = List[Tree]()) setPos a.pos
        }
      }
    } apply (ast)

    assertEquals("""
    trait tr {
      def bar(s: String) = s
      def foo() {
        bar()
      }
    }
    """, createText(result.get, Some(ast.pos.source)))
  }
  
  @Test
  def testPrintNoParentheses() {

    val ast = treeFrom("""
    trait tr {      
      def bar(s: String) = s
      def foo() {
        bar("ASD")
      }
    }
    """)

    val result = topdown {
      matchingChildren {
        transform {
          case a: Apply => a.fun
        }
      }
    } apply (ast)

    assertEquals("""
    trait tr {      
      def bar(s: String) = s
      def foo() {
        bar
      }
    }
    """, createText(result.get, Some(ast.pos.source)))
  }

  @Test
  def replaceWithThis() {

    val str = """
    class MyList[T] {
      //transformation works correctly when parentheses are missing here 
      def emptyp() = false
    }        
    trait tr[A] {
      self: KIVList[A] =>
      def every[A1](p: (A1) => Boolean, li: MyList[A]): Boolean = {
        li.emptyp || li.emptyp
      }
    }
    """
    val ast = treeFrom(str)
    
    var valDef: ValDef = null
    topdown {
      matchingChildren {
        transform {
          case t: ValDef =>
            valDef = t
            t
        }
      }
    } apply (ast)

    val result = topdown {
      matchingChildren {
        transform {
          case d: DefDef if (d.name.toString() == "every") => d.copy() replaces d
          case t: Ident if (t.symbol == valDef.symbol) =>
            Ident(newTypeName("this")) setPos t.pos
        }
      }
    } apply (ast)


    val changes = refactor(result.toList)
    val res = common.Change.applyChanges(changes, str)
    assertEquals("""
    class MyList[T] {
      //transformation works correctly when parentheses are missing here 
      def emptyp() = false
    }        
    trait tr[A] {
      self: KIVList[A] =>
      def every[A1](p: (A1) => Boolean, li: MyList[A]): Boolean = {
        this.emptyp || this.emptyp
      }
    }
    """, res)
  }
  
    @Test
  def changeMethodInvocation() {

    val str = """
    package abc
    object primitive {
      def mapcar[A, B](fu: (A) => B, li: List[A]): List[B] = {
        
      }
      def asd() = {
        mapcar(((x: String) => x.isEmpty()), List(""))
      }
    }
    """
    val ast = treeFrom(str)

    val result = topdown {
      matchingChildren {
        transform {
          case a: Apply if (a.args.length > 1) =>
            val fun1 = Select(
              name = newTermName(a.fun.symbol.nameString),
              qualifier = a.args.last)
            a.copy(args = a.args.init, fun = fun1) replaces a
        }
      }
    } apply (ast)

    val changes = refactor(result.toList)
    val res = common.Change.applyChanges(changes, str)
    assertEquals("""
    package abc
    object primitive {
      def mapcar[A, B](fu: (A) => B, li: List[A]): List[B] = {
        
      }
      def asd() = {
        List("").mapcar(((x: String) => x.isEmpty()))
      }
    }
    """, res)
  }

  @Test
  def changeMethodInvocation2() {

    val src = """
    package abc
    object primitive {
      def append[A](li1: List[A], li2: List[A]) = Nil
  
      append(List("asd"), {
          val x = 1
          List("Def")
      })
      
    }
    """
    
    val ast = treeFrom(src)

    val result = topdown {
      matchingChildren {
        transform {
          case a: Apply if (a.args.length > 1) =>
            val fun1 = Select(
              name = newTermName(a.fun.symbol.nameString),
              qualifier = a.args.last)
            a.copy(args = a.args.init, fun = fun1) replaces a
        }
      }
    } apply (ast)
    
    assertEquals("""
    package abc
    object primitive {
      def append[A](li1: List[A], li2: List[A]) = Nil
  
      {
          val x = 1
          List("Def")
      }.append(List("asd"))
      
    }
    """, createText(result.get, Some(ast.pos.source)))
  }   
  
  @Test
  def testRemoveAnArgument() {

    val src = """
    trait tr {
      def remove[A](elem: A, li: List[A]): List[A] = {
        li.remove_if((x: A) => (elem equals x))
      }
    }
    """
      
    val ast = treeFrom(src)

    val result = topdown {
      matchingChildren {
        transform {
          case t: DefDef if (t.vparamss(0).size > 0) =>
            val vparamss = List(List(t.vparamss(0).head))
            t.copy(vparamss = vparamss) setPos t.pos
        }
      }
    } apply (ast)

    val changes = refactor(result.toList)

    assertEquals("""
    trait tr {
      def remove[A](elem: A): List[A] = {
        li.remove_if((x: A) => (elem equals x))
      }
    }
    """, common.Change.applyChanges(changes, src))
  }
  
   @Test
  def testParentheses() {
    val ast = treeFrom("""
    package abc
    trait tr[T] {
      def remove[A](elem: A, li: List[A]): List[A] = {
        li.filterNot((x: A) => (elem equals x))
      }
    }
    """)
    val result = topdown {
      matchingChildren {
        transform {
          case t: DefDef if (t.name == newTermName("remove")) =>
            t.copy() replaces t
        }
      }
    } apply (ast)

    assertEquals("""
    package abc
    trait tr[T] {
      def remove[A](elem: A, li: List[A]): List[A] = {
        li.filterNot((x: A) => (elem equals x))
      }
    }
    """, createText(result.get, Some(ast.pos.source)))
  }
   
   @Test
  def testFullQualifiedName() {
    val str = """
    package asd
    object primitive {
      def member[A](a:A, li:List[A]):Boolean = true
      def has_no_duplicates[A](li: List[A]): Boolean = {
        li.isEmpty || ((!member(li.head, li.tail)) && has_no_duplicates(li.tail))
      }
    }
    """
    val ast = treeFrom(str)

    val result = topdown {
      matchingChildren {
        transform {
          case t: Template =>
            t.copy(body = t.body ::: List(t.body.last)) setPos t.pos
          case t @ TypeApply(Select(name, qualifier), args) if (t.symbol.name == newTermName("member")) =>
            val s = t.symbol.fullName
            val qual = s.substring(0, s.lastIndexOf("."))
            Select(
              qualifier = Ident(name = newTypeName(qual)),
              name = t.fun.asInstanceOf[Select].name)
        }
      }
    } apply ast
    val changes = refactor(result.toList)
    val res1 = common.Change.applyChanges(changes, str)
    assertEquals("""
    package asd
    object primitive {
      def member[A](a:A, li:List[A]):Boolean = true
      def has_no_duplicates[A](li: List[A]): Boolean = {
        li.isEmpty || ((!(asd.primitive.member(li.head, li.tail))) && has_no_duplicates(li.tail))
      }
      def has_no_duplicates[A](li: List[A]): Boolean = {
        li.isEmpty || ((!(asd.primitive.member(li.head, li.tail))) && has_no_duplicates(li.tail))
      }
    }
    """, res1)
  }
   
    @Test
  def changeMethodInvocation3() {

    val ast = treeFrom("""
    package abc
    object primitive {
      def append[A](li1: List[A], li2: List[A]) = Nil
      append(List("asd")
          , List("Def"))
    }
    """)

    val result = topdown {
      matchingChildren {
        transform {

          case a: Apply if (a.args.length > 1) =>
            val buf = a.args.toBuffer
            val arg = buf(1)
            buf.remove(1)
            val fun1 = Select(
              name = newTermName(a.fun.symbol.nameString),
              qualifier = arg)
            a.copy(args = buf.toList, fun = fun1) setPos a.pos
        }
      }
    } apply (ast)

    assertEquals("""
    package abc
    object primitive {
      def append[A](li1: List[A], li2: List[A]) = Nil
      
      List("Def").append(List("asd"))
    }
    """, createText(result.get, Some(ast.pos.source)))
  }
  
  @Test
  def testCopy() {
    val str = """
    object primitive {
      def member[A](a:A, li:List[A]):Boolean = {
        def f[B](li: List[B]) = {
          true
        }
        f[A](li)
        true
      }
    }
    """
    val ast = treeFrom(str)

    val result = topdown {
      matchingChildren {
        transform {
          case t: DefDef => t.copy()
        }
      }
    } apply ast

    val changes = refactor(result.toList)
    val res1 = common.Change.applyChanges(changes, str)
    assertEquals("""
    object primitive {
      def member[A](a:A, li:List[A]):Boolean = {
        def f[B](li: List[B]) = {
        true
        }
        f[A](li)
        true
      }
    }
    """, res1)
  }
  
  @Test
  def testCopy2() {
    val str = """
    package abc
    case class Pair[A, B](a: A, b: B)
    object primitive {
      def divide[A](testpred: (A) => Boolean): Pair[List[A], List[A]] = {
        null
      }
    }
    trait tr[A] {
      self: List[A] =>
    }
    """
    val ast = treeFrom(str)
    var defdef: Tree = null

    topdown(matchingChildren(
      transform {
        case t: DefDef if (t.name == newTermName("divide")) =>
          defdef = t; t
      })) apply ast

    val res = topdown(matchingChildren(
      transform {
        case t: TypeTree if (t.nameString == "A") =>
          mkRenamedTypeTree(t, "A1", t.symbol)
        case t: TypeDef if (t.nameString == "A") =>
          mkRenamedSymTree(t, "A1")
      })) apply defdef

    val result = topdown(matchingChildren(
      transform {
        case c: ClassDef if (c.name == newTypeName("tr")) =>
          val t = c.impl
          val templ = c.impl.copy(body = t.body ::: List(res.get)) setPos t.pos
          c.copy(impl = templ) setPos c.pos
      })) apply ast

    assertEquals("""
    package abc
    case class Pair[A, B](a: A, b: B)
    object primitive {
      def divide[A](testpred: (A) => Boolean): Pair[List[A], List[A]] = {
        null
      }
    }
    trait tr[A] {
      self: List[A] =>
      def divide[A1](testpred: (A1) => Boolean): Pair[List[A1], List[A1]] = {
        null
      }
    }
    """, common.Change.applyChanges(refactor(result.toList), str))
  }
  
  @Test
  def testCopy3() {
    val str = """
    package abc
    object primitive {
      def reduce1[A1, A](fu: (A1, A) => A1, li: List[A], init: A1) = init
      def member_test[A1, B](x: A1, li: List[B], testp: (A1, B) => Boolean): Boolean = {
        reduce1((b: Boolean, xli: B) => b, li, false)
      }
    }
    trait tr[A] {
      self: List[A] =>
    }
    """
    val ast = treeFrom(str)
    var defdef: Tree = null

    topdown(matchingChildren(
      transform {
        case t: DefDef if (t.name == newTermName("member_test")) =>
          defdef = t; t
      })) apply ast

    val res = topdown(matchingChildren(
      transform {
        case t: TypeTree if (t.nameString == "B") =>
          mkRenamedTypeTree(t, "B1", t.symbol)
        case t: TypeDef if (t.nameString == "B") =>
          mkRenamedSymTree(t, "B1")
      })) apply defdef

    val result = topdown(matchingChildren(
      transform {
        case c: ClassDef if (c.name == newTypeName("tr")) =>
          val t = c.impl
          val templ = c.impl.copy(body = t.body ::: List(res.get)) setPos t.pos
          c.copy(impl = templ) setPos c.pos
      })) apply ast

    assertEquals("""
    package abc
    object primitive {
      def reduce1[A1, A](fu: (A1, A) => A1, li: List[A], init: A1) = init
      def member_test[A1, B](x: A1, li: List[B], testp: (A1, B) => Boolean): Boolean = {
        reduce1((b: Boolean, xli: B) => b, li, false)
      }
    }
    trait tr[A] {
      self: List[A] =>
      def member_test[A1, B1](x: A1, li: List[B1], testp: (A1, B1) => Boolean): Boolean = {
        reduce1((b: Boolean, xli: B1) => b, li, false)
      }
    }
    """, common.Change.applyChanges(refactor(result.toList), str))
  }
   
  @Test
  def testCopy4() {
    val str = """
    package abc
    object primitive {
      def fail() = {true}
      def foo(f: Int): Boolean = {
        if((f == 0)) fail
        else false
      }
    }
    """
    val ast = treeFrom(str)

    val result = topdown(matchingChildren(
      transform {
        case t: Apply if (t.fun.nameString == "fail") => t.copy()
      })) apply ast

    assertEquals("""
    package abc
    object primitive {
      def fail() = {true}
      def foo(f: Int): Boolean = {
        if((f == 0)) fail()
        else false
      }
    }
    """, common.Change.applyChanges(refactor(result.toList), str))
  }

  @Test
  def testCopy4Variation() {
    val str = """
    package abc
    object primitive {
      def fail() = {true}
      def foo(f: Int): Boolean = {
        if((f == 0)) fail()
        else false
      }
    }
    """
    val ast = treeFrom(str)

    val result = topdown(matchingChildren(
      transform {
        case t: DefDef => t.copy()
        case t: Apply =>
          if (t.fun.nameString == "fail") {
            val s = t.symbol.fullName
            val qual = s.substring(0, s.lastIndexOf("."))
            val select = Select(
              qualifier = Ident(name = newTypeName(qual)),
              name = t.fun.asInstanceOf[Select].name)
            t.copy(fun = select) setPos t.pos 
          } else {
            t
          }
        case t => t
      })) apply ast

    assertEquals("""
    package abc
    object primitive {
      def fail() = {true}
      
      def foo(f: Int): Boolean = {
      if((f == 0)) abc.primitive.fail()
      else false
      }
    }
    """, common.Change.applyChanges(refactor(result.toList), str))
  }
  
  @Test
  def testCopy5() {
    val str = """
    package abc
    object primitive {
      def length[A](li:List[A]):Int = 0
      def enumerate[A](li:List[A]):List[Int] = {
        length[A](li)
      Nil
      }
    }
    trait tr[A] {
      self: List[A] =>
    }
    """
    val ast = treeFrom(str)
    var defdef: Tree = null

    topdown(matchingChildren(
      transform {
        case t: DefDef if (t.name == newTermName("enumerate")) =>
          defdef = t; t
      })) apply ast

    val names = Map("A" -> "A1")
    val res = topdown(matchingChildren(
      transform {
        case t @ TypeApply(Select(name, qualifier), args) =>
          val s = t.symbol.fullName
          val qual = s.substring(0, s.lastIndexOf("."))
          val select = Select(
            qualifier = Ident(name = newTypeName(qual)),
            name = t.fun.asInstanceOf[Select].name)
          t.copy(fun = select, args = args) setPos t.pos
        case t: TypeTree if (names.contains(t.nameString)) =>
          mkRenamedTypeTree(t, names(t.nameString).toString(), t.symbol)
        case t: TypeDef if (names.contains(t.name.toString)) =>
          mkRenamedSymTree(t, names(t.name.toString))
      })) apply defdef

    val result = topdown(matchingChildren(
      transform {
        case c: ClassDef if (c.name == newTypeName("tr")) =>
          val t = c.impl
          val templ = c.impl.copy(body = t.body ::: List(res.get)) setPos t.pos
          c.copy(impl = templ) setPos c.pos
      })) apply ast

    assertEquals("""
    package abc
    object primitive {
      def length[A](li:List[A]):Int = 0
      def enumerate[A](li:List[A]):List[Int] = {
        length[A](li)
      Nil
      }
    }
    trait tr[A] {
      self: List[A] =>
      def enumerate[A1](li:List[A1]):List[Int] = {
        abc.primitive.length[A1](li)
      Nil
      }
    }
    """, common.Change.applyChanges(refactor(result.toList), str))
  }
  
  @Test
  def changeMethodInvocation4() {

    val ast = treeFrom("""
    package abc
    object primitive {
      def append[A](li1: List[A], li2: List[A]) = Nil
      append(List("asd"), if(true) List("A") else List("B"))
    }
    """)

    val result = topdown {
      matchingChildren {
        transform {

          case a: Apply if (a.args.length > 1) =>
            val buf = a.args.toBuffer
            val arg = buf(1)
            buf.remove(1)
            val fun1 = Select(
              name = newTermName(a.fun.symbol.nameString),
              qualifier = arg)
            a.copy(args = buf.toList, fun = fun1) setPos a.pos
        }
      }
    } apply (ast)

    assertEquals("""
    package abc
    object primitive {
      def append[A](li1: List[A], li2: List[A]) = Nil
      (if(true) List("A") else List("B")).append(List("asd"))
    }
    """, createText(result.get, Some(ast.pos.source)))
  }
  
  @Test
  def changeMethodInvocation5() {

   val str = """
    package abc
    object primitive {
      def remove_if_not[A](fun:(A) => Boolean, li0:List[A]):List[A] = Nil
      val opentries = remove_if_not((_:List[Int]).isEmpty, List(Nil))
    }
    """
    val ast = treeFrom(str)

    val result = topdown {
      matchingChildren {
        transform {
          case a: Apply if (a.args.length > 1) =>
            val buf = a.args.toBuffer
            val arg = buf(1)
            buf.remove(1)
            val fun1 = Select(
              name = newTermName(a.fun.symbol.nameString),
              qualifier = arg)
            a.copy(args = buf.toList, fun = fun1) setPos a.pos
        case a: Function =>
          val res = topdown {
            matchingChildren {
              transform {
                case a: Apply =>
                  a.symbol = NoSymbol
                  a
              }
            }
          } apply a
          res.get
        }
      }
    } apply (ast)
    
    val changes = refactor(result.toList)
    val res = common.Change.applyChanges(changes, str)
    assertEquals("""
    package abc
    object primitive {
      def remove_if_not[A](fun:(A) => Boolean, li0:List[A]):List[A] = Nil
      val opentries = List(Nil).remove_if_not((_:List[Int]).isEmpty)
    }
    """, res)
  }

  @Test
  def patternMatchTest() {
    val src = """
  object acmatch {
    def fail = throw new UnsupportedOperationException("unsupported")
    def toInline() = if((null equals null)) true
      else fail
    def acmatch_expr(x: Any) = x match {
      case t: List[_] => toInline()
      case false => fail
    }
  }
    """
    val ast = treeFrom(src).asInstanceOf[PackageDef]
    val toInline = ast.stats(0).asInstanceOf[ModuleDef]
      .impl.body(2).asInstanceOf[DefDef]
    
    def mkPattern(varName: String, className: String, guard: Tree, rhs: Tree): CaseDef = {
      CaseDef(Bind(newTermName("t"),
        if (className != "") Typed(Ident(newTermName("")), Ident(newTermName(className)))
        else EmptyTree),
        guard, rhs)
    }
    
    val result = topdown {
      matchingChildren {
        transform {
          case t: Match =>
            val rhs = toInline.rhs.asInstanceOf[If]
            val caseDef = mkPattern("", "ASD", EmptyTree, rhs.copy())
            t.copy(cases = t.cases ::: List(caseDef))
          }
      }
    } apply ast
    assertEquals("""
  object acmatch {
    def fail = throw new UnsupportedOperationException("unsupported")
    def toInline() = if((null equals null)) true
      else fail
    def acmatch_expr(x: Any) = x match {
      case t: List[_] => toInline()
      case false => fail
      case t: ASD => if ((null equals null)) true else fail
    }
  }
    """, Change.applyChanges(refactor(result.toList), src))
  }
  
  @Test
  def patternMatchTest2() {
    val src = """
  object acmatch {
    def fail = throw new UnsupportedOperationException("unsupported")
    def method() {
    }
    class ASD
    def toInline() = {
      if (true && true) {
        if (!true) {
          fail
        } else {
          println("asd")
          if (true && true) true
          else false
        }
      } else method()
    }
    def acmatch_expr(x: Any) = x match {
      case false => fail
    }
  }
  """
    val ast = treeFrom(src).asInstanceOf[PackageDef]
    val toInline = ast.stats(0).asInstanceOf[ModuleDef]
      .impl.body(4).asInstanceOf[DefDef]

    def mkPattern(varName: String, className: String, guard: Tree, rhs: Tree): CaseDef = {
      CaseDef(Bind(newTermName("t"),
        if (className != "") Typed(Ident(newTermName("")), Ident(newTermName(className)))
        else EmptyTree),
        guard, rhs)
    }

    val result = topdown {
      matchingChildren {
        transform {
          case t: DefDef if(t.name.toString() == "acmatch_expr")=>
            val rhs = toInline.rhs.asInstanceOf[If]
            val caseDef = mkPattern("", "ASD", EmptyTree, rhs.copy())
            val matchx = Match(Ident(newTermName("x")), List(caseDef))
            t.copy(rhs = matchx) replaces t
        }
      }
    } apply ast
    assertEquals("""
  object acmatch {
    def fail = throw new UnsupportedOperationException("unsupported")
    def method() {
    }
    class ASD
    def toInline() = {
      if (true && true) {
        if (!true) {
          fail
        } else {
          println("asd")
          if (true && true) true
          else false
        }
      } else method()
    }
    def acmatch_expr(x: Any) = x match {
      case t: ASD => if (true && true) {
        if (!true) {
          fail
        } else {
          println("asd")
          if (true && true) true
          else false
        }
      } else method()
    }
  }
  """, Change.applyChanges(refactor(result.toList), src))
  }
  
  @Test
  def patternMatchTest3() {
    val src = """
object acmatch {
    def fail = throw new UnsupportedOperationException("unsupported")
    def subst_tlop2(obj: Expr, selfun1: (Expr) => Expr, selfun2: (Expr) => Expr): Expr = {
      null
    }
    def subst_until(obj: Expr): Expr = {
      subst_tlop2(obj, ((_: Expr).fma1), ((_: Expr).fma2))
    }

    def subst_expr(obj: Expr): Expr = {
      if (obj.untilp) subst_until(obj)
      else fail
    }
    abstract class Expr(val fma1: Expr, val fma2: Expr) {
      def untilp = false
    }
    abstract class Until(fma1: Expr, fma2: Expr) extends Expr(fma1, fma2) {
      override def untilp = true
    }
  }
  """
    val ast = treeFrom(src).asInstanceOf[PackageDef]
    val toInline = ast.stats(0).asInstanceOf[ModuleDef]
      .impl.body(3).asInstanceOf[DefDef]

    def mkPattern(varName: String, className: String, guard: Tree, rhs: Tree): CaseDef = {
      CaseDef(Bind(newTermName("t"),
        if (className != "") Typed(Ident(newTermName("")), Ident(newTermName(className)))
        else EmptyTree),
        guard, rhs)
    }

    val result = topdown {
      matchingChildren {
        transform {
          case t: DefDef if (t.name.toString() == "subst_expr") =>
            val rhs = toInline.rhs.asInstanceOf[Apply]
            val caseDef = mkPattern("", "Until", EmptyTree, rhs.copy())
            val matchx = Match(Ident(newTermName("obj")), List(caseDef))
            t.copy(rhs = matchx) setPos t.pos
        }
      }
    } apply ast
    assertEquals("""
object acmatch {
    def fail = throw new UnsupportedOperationException("unsupported")
    def subst_tlop2(obj: Expr, selfun1: (Expr) => Expr, selfun2: (Expr) => Expr): Expr = {
      null
    }
    def subst_until(obj: Expr): Expr = {
      subst_tlop2(obj, ((_: Expr).fma1), ((_: Expr).fma2))
    }

    def subst_expr(obj: Expr): Expr = obj match {
      case t: Until => subst_tlop2(obj, ((_: Expr).fma1), ((_: Expr).fma2))
    }
    abstract class Expr(val fma1: Expr, val fma2: Expr) {
      def untilp = false
    }
    abstract class Until(fma1: Expr, fma2: Expr) extends Expr(fma1, fma2) {
      override def untilp = true
    }
  }
  """, Change.applyChanges(refactor(result.toList), src))
  }
  
  @Test
  def patternMatchTest4() {
    val src = """
object acmatch {
    def fail = throw new UnsupportedOperationException("unsupported")
    def find[A](f: A => Boolean, li: List[A]) = li.head
    def compile_apply_substitution_on_abstractionmv(obj: Abstraction): (List[Mvmatch]) => Abstraction = {
      (subst: List[Mvmatch]) =>
        find((mvmatch: Mvmatch) => true, subst).abstractionmatchabstraction
    }
    def subst_expr(obj: Abstraction): (List[Mvmatch]) => Abstraction = {
      if (obj.abstractionmvp) compile_apply_substitution_on_abstractionmv(obj)
      else fail 
    }
    abstract class Abstraction {
      def abstractionmvp = false
    }
    class Abstractionmv extends Abstraction {
      override def abstractionmvp = true
    }
    class Mvmatch(val abstractionmatchmv: Abstraction, val abstractionmatchabstraction: Abstraction) {
      def abstractionmatchp = true
    }
  }
  """
    val ast = treeFrom(src).asInstanceOf[PackageDef]
    val toInline = ast.stats(0).asInstanceOf[ModuleDef]
      .impl.body(3).asInstanceOf[DefDef]

    def mkPattern(varName: String, className: String, guard: Tree, rhs: Tree): CaseDef = {
      CaseDef(Bind(newTermName("t"),
        if (className != "") Typed(Ident(newTermName("")), Ident(newTermName(className)))
        else EmptyTree),
        guard, rhs)
    }

    val result = topdown {
      matchingChildren {
        transform {
          case t: DefDef if (t.name.toString() == "subst_expr") =>
            val rhs = toInline.rhs.asInstanceOf[Function]
            val caseDef = mkPattern("", "Abstractionmv", EmptyTree, rhs.copy())
            val matchx = Match(Ident(newTermName("obj")), List(caseDef))
            t.copy(rhs = matchx) setPos t.pos
        }
      }
    } apply ast
    assertEquals("""
object acmatch {
    def fail = throw new UnsupportedOperationException("unsupported")
    def find[A](f: A => Boolean, li: List[A]) = li.head
    def compile_apply_substitution_on_abstractionmv(obj: Abstraction): (List[Mvmatch]) => Abstraction = {
      (subst: List[Mvmatch]) =>
        find((mvmatch: Mvmatch) => true, subst).abstractionmatchabstraction
    }
    def subst_expr(obj: Abstraction): (List[Mvmatch]) => Abstraction = obj match {
      case t: Abstractionmv => (subst: List[Mvmatch]) =>
      find((mvmatch: Mvmatch) => true, subst).abstractionmatchabstraction
    }
    abstract class Abstraction {
      def abstractionmvp = false
    }
    class Abstractionmv extends Abstraction {
      override def abstractionmvp = true
    }
    class Mvmatch(val abstractionmatchmv: Abstraction, val abstractionmatchabstraction: Abstraction) {
      def abstractionmatchp = true
    }
  }
  """, Change.applyChanges(refactor(result.toList), src))
  }
  
  @Test
  def changeMethodInvocation7() {

    val str = """
    package abc
    object primitive {
      def length[A](li: List[A]) = 0
      def reduce[A, B](li: List[B], fu: (A, B) => A, init: A): A = throw new Exception("ASD")
      def foo[A](li: List[Int], li2: List[Int]) = {  
        reduce(li, (a: Int, b: Int) => {
        length(li2) + b}, 0)
      }
    }
    """
    val ast = treeFrom(str)
    val result = topdown {
      matchingChildren {
        transform {
          case a @ Apply(fun: TypeApply, _) =>
            val buf = a.args.toBuffer
            val arg = buf(0)
            buf.remove(0)
            val fun1 = Select(
              name = newTermName(a.fun.symbol.nameString),
              qualifier = arg)
            a.copy(args = buf.toList, fun = fun1) setPos a.pos
        }
      }
    } apply (ast)
    val changes = refactor(result.toList)
    val res = Change.applyChanges(changes, str)
    assertEquals("""
    package abc
    object primitive {
      def length[A](li: List[A]) = 0
      def reduce[A, B](li: List[B], fu: (A, B) => A, init: A): A = throw new Exception("ASD")
      def foo[A](li: List[Int], li2: List[Int]) = {  
        li.reduce((a: Int, b: Int) => {
        li2.length() + b}, 0)
      }
    }
    """, res)
  }
  
  @Test
  def changeMethodInvocation8() {

    val str = """
    object primitive {
      def reduce[A, B](fu: (A, B) => A, li: List[B], init: A): A = init
      def asd(li: List[Int]) = {
        reduce(((_:Int) + (_:Int)),li, 0) 
      }
    }
    """
    val ast = treeFrom(str)

    val result = topdown {
      matchingChildren {
        transform {
          case a: Apply if (a.args.length > 1) =>
            val buf = a.args.toBuffer
            val arg = buf(1)
            buf.remove(1)
            val fun1 = Select(
              name = newTermName(a.fun.symbol.nameString),
              qualifier = arg)
            a.copy(args = buf.toList, fun = fun1) setPos a.pos
        }
      }
    } apply (ast)

    val changes = refactor(result.toList)
    val res = Change.applyChanges(changes, str)
    assertEquals("""
    object primitive {
      def reduce[A, B](fu: (A, B) => A, li: List[B], init: A): A = init
      def asd(li: List[Int]) = {
        li.reduce(((_:Int) + (_:Int)), 0) 
      }
    }
    """, res)
  }
    
  @Test
  def testAddSealed() {
    val str = """
    abstract class asd {
    }
    class XY
    """
    val ast = treeFrom(str)
    val res = once {
      transform {
        case t: PackageDef =>
          val st = t.stats.map(a => a match {
            case x: ClassDef =>
              x.copy(mods = x.mods.withPosition(Flags.SEALED, NoPosition)) setPos x.pos
            case x => x
          })
          t.copy(stats = st) setPos t.pos
      }} apply ast

    assertEquals("""
    abstract sealed class asd {
    }
    sealed class XY
    """, Change.applyChanges(refactor(res.toList), str))
  }
  
  @Test
  def testRemoveSealed() {
    val str = """
    abstract sealed class asd {
    }
    sealed class XY
    """
    val ast = treeFrom(str)
    val res = topdown {
      matchingChildren {
        transform {
          case x: ClassDef => x.copy(mods = NoMods) replaces x
        }
      }
     } apply ast

    assertEquals("""
    class asd {
    }
    class XY
    """, Change.applyChanges(refactor(res.toList), str))
  }
      
  @Test
  def testAddSealedPP() {
    val str = """
    abstract class asd {
    }
    class XY
    """
    val ast = treeFrom(str)
    val res = once {
      transform {
        case t: PackageDef =>
          val st = t.stats.map(a => a match {
            case x: ClassDef =>
              x.copy(mods = x.mods.withPosition(Flags.SEALED, NoPosition))
            case x => x
          })
          t.copy(stats = st) setPos t.pos
      }} apply ast

    assertEquals("""
    abstract sealed class asd {
    }
    
    sealed class XY
    """, Change.applyChanges(refactor(res.toList), str))
  }
  
  @Test
  def testRemoveSealedPP() {
    val str = """
    abstract sealed class asd {
    }
    sealed class XY
    """
    val ast = treeFrom(str)
    val res = topdown {
      matchingChildren {
        transform {
          case x: ClassDef => x.copy(mods = NoMods)
        }
      }
     } apply ast

    assertEquals("""
    class asd {
    }
    
    class XY
    """, Change.applyChanges(refactor(res.toList), str))
  }
  
  @Test
  def insertPlainString() {
    val source = """
    class InsertHere {
      def method = {
        val list = List(1,2,3) 
        list map (i => i * 2)
      }
    }
    """
    val ast = treeFrom(source)
    val transformed = topdown {
      matchingChildren {
        transform {
          case t @ Block(stmts, expr) =>
            val string = PlainText.Indented("""
               |for (
               |  i <- list
               |) yield {
               |  i * 2
               |}""".stripMargin)
            
            t.copy(expr = string)
        }
      }
     } apply ast

    assertEquals("""
    class InsertHere {
      def method = {
        val list = List(1,2,3) 
        for (
          i <- list
        ) yield {
          i * 2
        }
      }
    }
    """, Change.applyChanges(refactor(transformed.toList), source))
  }
}
