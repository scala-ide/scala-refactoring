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

class IndividualSourceGenTest extends TestHelper with SourceGenerator with SilentTracing with transformation.TreeFactory {
  
  import global._
  
  override def treeForFile(file: AbstractFile) = {
    unitOfFile get file map (_.body) flatMap removeAuxiliaryTrees
  }
    
  def generateText(t: Tree): String = generate(t, sourceFile = Some(t.pos.source)).asText
  
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
        
    assertEquals(exp1, generate(removeAuxiliaryTrees apply newDefDef get, sourceFile = Some(originalDefDef.pos.source)).asText)

    newDefDef.setPos(originalDefDef.pos)
    
    assertEquals(exp2, generate(removeAuxiliaryTrees apply newDefDef get, sourceFile = Some(originalDefDef.pos.source)).asText)    
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
    val defdef = mkDefDef(name = "member", body = List(Ident("()")))
    
    val transformedAst = topdown {
      matchingChildren {
        transform {
          case t: Template => t.copy(body = t.body ::: List(defdef)) setPos t.pos
        }
      }
    } apply ast
    
    assertEquals("""{
  self: List[A] => 
  
  def asd() {
  }
  
  def member() = {
    ()
  }
}""", refactor(transformedAst.toList).head.text)
  }
  
  @Test
  def addMethodToEmptyTraitWithSelfTypeAnnotation {
    val src = """
trait tr[A] {
  self: List[A] => 

}
"""
    val ast = treeFrom(src)
    
    val defdef = mkDefDef(name = "member", body = List(Ident("()")))
    
    val transformedAst = topdown {
      matchingChildren {
        transform {
          case t: Template => t.copy(body = t.body ::: List(defdef)) setPos t.pos
        }
      }
    } apply ast
    
    assertEquals("""{
  self: List[A] => 
  def member() = {
    ()
  }

}""", refactor(transformedAst.toList).head.text)
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
    assertEquals("""def member[A](a: A, li: KIVList[A]): Boolean = {
if (li.emptyp) false
else {
  (a equals li.car) || member(a, li.cdr)
}
        }""", changes.head.text)
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
      case _ => Assert.fail(); Predef.error("") // too bad fail does not return Nothing
    }
    
    val newRHS1 = Apply(Select(Ident("com"),"synchronized"), List(shallowDuplicate(originalDefDef.rhs) setPos NoPosition))
    
    val newDefDef1 = originalDefDef copy (rhs = newRHS1) setPos originalDefDef.pos
        
    assertEquals("""def amethod(v: Int, a: Account): Unit = com.synchronized(a.add(v))""", generate(removeAuxiliaryTrees apply newDefDef1 get, sourceFile = Some(originalDefDef.pos.source)).center.asText)
   
    assertEquals("""
       def amethod(v: Int, a: Account): Unit = com.synchronized(a.add(v))""", createFragment(removeAuxiliaryTrees apply newDefDef1 get).asText)
   
    val newRHS2 = Apply(Select(Ident("com"),"synchronized"), List(originalDefDef.rhs))
    
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
      case _ => Assert.fail(); Predef.error("") // too bad fail does not return Nothing
    }
    
    val newRHS1 = new Block(List(Apply(Select(Ident("com"),"synchronized"), List(originalDefDef.rhs))), EmptyTree)
    
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

class RunWith(c: Class[_]) extends StaticAnnotation

/*(*/  /*)*/

@RunWith(classOf[String])
class Test
    """)
      
    assertEquals("""
import java.lang.String

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
              name = a.fun.symbol.nameString,
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
        List("").mapcar((x: String) => x.isEmpty())
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
              name = a.fun.symbol.nameString,
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

    val ast = treeFrom("""
    trait tr {
      def remove[A](elem: A, li: List[A]): List[A] = {
        li.remove_if((x: A) => (elem equals x))
      }
    }
    """)

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
    val res =
      assertEquals(
        """def remove[A](elem: A): List[A] = {
        li.remove_if((x: A) => (elem equals x))
      }""", changes.head.text)
  }
  
   @Test
  def testParentheses() {
    val ast = treeFrom("""
    package abc
    trait tr[T] {
      def remove[A](elem: A, li: List[A]): List[A] = {
        li.remove((x: A) => (elem equals x))
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
        li.remove((x: A) => (elem equals x))
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
}

