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

class IndividualSourceGenTest extends TestHelper with SourceGenerator with SilentTracing {
  
  import global._
  
  override def treeForFile(file: AbstractFile) = {
    unitOfFile get file map (_.body) flatMap removeAuxiliaryTrees
  }
    
  def generateText(t: Tree): String = generate(t).asText
  
  implicit def treeToPrettyPrint(original: Tree) = new {
    def cleanTree(t: Tree) = (removeAuxiliaryTrees &> emptyAllPositions)(t).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generateText(cleanTree(original)))
  }
  
  def getFirstMethod(source: String) = treeFrom(source) match {
    case PackageDef(_, ClassDef(_, _, _, Template(_, _, (d: DefDef) :: _)) :: _) => d
    case _ => Assert.fail(); throw new Exception("unreachable")
  }
  
  def testDefDefWithNoPositionAndOriginalPosition(src: String, exp1: String, exp2: String) = {
    
    val originalDefDef = getFirstMethod(src)
    
    val newDefDef = originalDefDef.copy()
        
    assertEquals(exp1, generateText(removeAuxiliaryTrees apply newDefDef get))

    newDefDef.setPos(originalDefDef.pos)
    
    assertEquals(exp2, generateText(removeAuxiliaryTrees apply newDefDef get))    
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
    """def someMethod: String {
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
  def valDefRhsAlone(): Unit = {
    
    val valDef = treeFrom("""
    object O {
      val a = {4 + 3  }
    }
    """) match {
      case PackageDef(_, ModuleDef(_, _, Template(_, _, _ :: (v: ValDef) :: _)) :: _) => v
      case _ => Assert.fail(); emptyValDef // too bad fail does not return Nothing
    }
    
    val p = valDef.symbol.isPrivateLocal
    
    assertEquals("""val a = {4 + 3  }""", generate(removeAuxiliaryTrees apply valDef get).center.asText)
      
    assertEquals("""{4 + 3  }""", generate(removeAuxiliaryTrees apply valDef.rhs get).asText)
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
  }
  
  @Test
  def doubledName(): Unit = {
    
    val tree = treeFrom("""
class Account {
   var value = 0
   def add1(v: Int) {
     value += v
   }
}
    """)
      
    assertEquals("""
class Account {
   var value = 0
   def add1(v: Int) {
     value += v
   }
}
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
  
  @Test
  def extraParens(): Unit = {
    
    val tree = treeFrom("""
object Account {
   def amethod(v: Int, a: Int) = 
     synchronized({
       v + a
     })
}
    """)
      
    assertEquals("""
object Account {
   def amethod(v: Int, a: Int) = 
     synchronized({
       v + a
     })
}
    """, generateText(removeAuxiliaryTrees apply tree get))
  }
}

