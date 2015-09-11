/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports
import tests.util.TestHelper
import tests.util.TestRefactoring
import language.reflectiveCalls

class OrganizeImportsTest extends OrganizeImportsBaseTest {

  private def organize(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters()
  }.mkChanges

  private def organizeWithoutCollapsing(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(options = List(refactoring.SortImportSelectors))
  }.mkChanges

  private def organizeExpand(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new refactoring.RefactoringParameters(options = List(refactoring.ExpandImports, refactoring.SortImports))
  }.mkChanges

  private def organizeWithTypicalParams(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = {
      val groupImports = refactoring.GroupImports(List("java", "scala", "org", "com"))
      val alwaysUseWildcards = refactoring.AlwaysUseWildcards(Set("scalaz", "scalaz.Scalaz"))

      new refactoring.RefactoringParameters(
          options =
            refactoring.ExpandImports ::
            refactoring.SortImports ::
            groupImports ::
            alwaysUseWildcards ::
            refactoring.PrependScalaPackage ::
            Nil,
          deps = refactoring.Dependencies.FullyRecompute)
    }
  }.mkChanges

  @Test
  def testOrganizeOptions() {

    val src = """
      package tests.importing

      import scala.collection.mutable.{ListBuffer, HashMap}
      import scala.io.Source
      import scala.math.BigInt
      import scala.math._

      import scala.util.{Properties => ScalaProperties}
      """

    val restOfFile = """
      object Main {
        // we need to actually use the imports, otherwise they are removed
        val lb = ListBuffer(1)
        val lb = HashMap(1 → 1)
        var no: Source.type = null
        var elem: Source = null
        var bigi: BigInt = null
        var bigd: BigDecimal = null
        var props: ScalaProperties = null
      }
      """

    new FileSet {
      (src + restOfFile) becomes
      """
      package tests.importing

      import scala.collection.mutable.{HashMap, ListBuffer}
      import scala.io.Source
      import scala.math._
      import scala.math.BigInt
      """ + restOfFile
    } applyRefactoring organizeWithoutCollapsing

    new FileSet {
      (src + restOfFile) becomes
      """
      package tests.importing

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source
      import scala.math._
      import scala.math.BigInt
      """ + restOfFile
    } applyRefactoring organizeExpand

    new FileSet {
      (src + restOfFile) becomes
      """
      package tests.importing

      import scala.collection.mutable.{HashMap, ListBuffer}
      import scala.io.Source
      import scala.math._
      """ + restOfFile
    } applyRefactoring organize
  }

  @Test
  def expandImports() = new FileSet {
    """
      package tests.importing

      import scala.collection.mutable.{ListBuffer, HashMap}

      object Main {val lb = ListBuffer(1); val lb = HashMap(1 → 1) }
    """ becomes
    """
      package tests.importing

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      object Main {val lb = ListBuffer(1); val lb = HashMap(1 → 1) }
    """
  } applyRefactoring organizeExpand

  @Test
  def expandImportsButNotWildcards() = new FileSet {
    """
      package tests.importing

      import scala.collection.mutable.{ListBuffer => LB, _}

      object Main {val lb = LB(1) }
    """ becomes
    """
      package tests.importing

      import scala.collection.mutable.{ListBuffer => LB, _}

      object Main {val lb = LB(1) }
    """
  } applyRefactoring organizeExpand

  @Test
  def dontCollapseImports() = new FileSet {
    """
      package tests.importing

      import scala.collection.mutable.ListBuffer
      import scala.collection.mutable.HashMap

      object Main {val lb = ListBuffer(1); val lb = HashMap(1 → 1) }
    """ becomes
    """
      package tests.importing

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      object Main {val lb = ListBuffer(1); val lb = HashMap(1 → 1) }
    """
  } applyRefactoring organizeWithoutCollapsing

  @Test
  def sort() = new FileSet {
    """
      package tests.importing

      import scala.collection.mutable.ListBuffer
      import java.lang.Object

      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1)}
    """ becomes
    """
      package tests.importing

      import java.lang.Object
      import scala.collection.mutable.ListBuffer

      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1)}
    """
  } applyRefactoring organize

  @Test
  def collapse() = new FileSet {
    """
      import java.lang.String
      import java.lang.Object

      object Main {val s: String = ""; var o: Object = null}
    """ becomes
    """
      import java.lang.{Object, String}

      object Main {val s: String = ""; var o: Object = null}
    """
  } applyRefactoring organize

  @Test
  def sortSelectors() = new FileSet {
    """
      import java.lang.{String, Object}

      object Main {val s: String = ""; var o: Object = null}
    """ becomes
    """
      import java.lang.{Object, String}

      object Main {val s: String = ""; var o: Object = null}
    """
  } applyRefactoring organize

  @Test
  def sortAndCollapse() = new FileSet {
    """
      import scala.collection.mutable.ListBuffer
      import java.lang.String
      import java.lang.Object

      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1)}
    """ becomes
    """
      import java.lang.{Object, String}
      import scala.collection.mutable.ListBuffer

      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1)}
    """
  } applyRefactoring organize

  @Test
  def collapseWithRename() = new FileSet {
    """
      import java.lang.{String => S}
      import java.lang.{Object => Objekt}

      object Main {val s: String = ""; var o: Objekt = null}
    """ becomes
    """
      import java.lang.{Object => Objekt, String => S}

      object Main {val s: String = ""; var o: Objekt = null}
    """
  } applyRefactoring organize

  @Test
  def removeOneFromMany() = new FileSet {
    """
      import java.lang.{String, Math}

      object Main {val s: String = ""}
    """ becomes
    """
      import java.lang.String

      object Main {val s: String = ""}
    """
  } applyRefactoring organize

  @Test
  def importAll() = new FileSet {
    """
      import java.lang._
      import java.lang.String

      object Main
    """ becomes
    """
      import java.lang._

      object Main
    """
  } applyRefactoring organize

  @Test
  def importOnTrait() = new FileSet {
    """
      package importOnTrait
      import java.lang._
      import java.lang.String

      trait A

      trait Main extends A {
      }
    """ becomes
    """
      package importOnTrait

      import java.lang._

      trait A

      trait Main extends A {
      }
    """
  } applyRefactoring organize

  @Test
  def importWithSpace() = new FileSet {
    """

      import scala.collection.mutable.ListBuffer
      import java.lang.String

      object Main { val s: String = ""; val lb = ListBuffer("") }
    """ becomes
    """

      import java.lang.String
      import scala.collection.mutable.ListBuffer

      object Main { val s: String = ""; val lb = ListBuffer("") }
    """
  } applyRefactoring organize

  @Test
  def importAllWithRename() = new FileSet {
    """
      import java.lang._
      import java.lang.{String => S}

      object Main { val s: String = "" }
    """ becomes
    """
      import java.lang.{String => S, _}

      object Main { val s: String = "" }
    """
  } applyRefactoring organize

  @Test
  def importRemovesUnneeded() = new FileSet {
    """
      import java.lang._
      import java.lang.{String => S}
      import java.util.Map
      import scala.io.Source
      import scala.collection.mutable.ListBuffer

      object Main {
        val s: String = ""
        val l = ListBuffer(1,2,3)
        val l2 = List(1,2,3)
      }
    """ becomes
    """
      import java.lang.{String => S, _}
      import scala.collection.mutable.ListBuffer

      object Main {
        val s: String = ""
        val l = ListBuffer(1,2,3)
        val l2 = List(1,2,3)
      }
    """
  } applyRefactoring organize

  @Test
  def multipleImportsOnOneLine() = new FileSet {
      """
      import java.lang.String, String._

      object Main {
        val s: String = ""
        val s1 = valueOf(2);
      }    """ becomes """
      import java.lang.String
      import java.lang.String._

      object Main {
        val s: String = ""
        val s1 = valueOf(2);
      }    """
  } applyRefactoring organize

  @Test
  def importsInNestedPackages() = new FileSet {
    """
       package outer
       package inner

       import scala.collection.mutable.ListBuffer
       import scala.collection.mutable.HashMap

       object Main {
         var hm: HashMap[String, String] = null
       }
      """ becomes """
       package outer
       package inner

       import scala.collection.mutable.HashMap

       object Main {
         var hm: HashMap[String, String] = null
       }
      """
  } applyRefactoring organize

  @Test
  def importFromPackageObject() = new FileSet {
    """
    import scala.collection.breakOut
    import scala.collection.mutable.ListBuffer

    object TestbreakOut {
      val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
    }
    """ becomes """
    import scala.collection.breakOut

    object TestbreakOut {
      val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
    }
    """
  } applyRefactoring organize

  @Test
  def unusedImportWildcards() = new FileSet {
    """
      import java.util._
      import scala.collection._

      object Main {
      }    """ becomes
    """
      ▒

      object Main {
      }    """
  } applyRefactoring organize

  @Test
  def simplifyWildcards() = new FileSet {
    """
      import scala.collection.mutable._
      import scala.collection.mutable.ListBuffer

      object Main {
        var x: ListBuffer = null
      }    """ becomes
    """
      import scala.collection.mutable._

      object Main {
        var x: ListBuffer = null
      }    """
  } applyRefactoring organize

  @Test
  def appliedType() = new FileSet {
    """
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      trait SomeTrait {
        def m: Either[String, ListBuffer[ListBuffer[String]]]
      }    """ becomes
    """
      import scala.collection.mutable.ListBuffer

      trait SomeTrait {
        def m: Either[String, ListBuffer[ListBuffer[String]]]
      }    """
  } applyRefactoring organize

  @Test
  def annotateClass() = new FileSet {
    """
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      @annotation.implicitNotFound(msg = "message")
      trait SomeTraitWithAnAnnotation {
        def m: Either[String, ListBuffer[ListBuffer[String]]]
      }    """ becomes
    """
      import scala.collection.mutable.ListBuffer

      @annotation.implicitNotFound(msg = "message")
      trait SomeTraitWithAnAnnotation {
        def m: Either[String, ListBuffer[ListBuffer[String]]]
      }    """
  } applyRefactoring organize

  @Test
  def importSymbolicName() = new FileSet {
    """
      import collection.immutable.Nil.++

      object YYY {

        ++(Nil)

      }
    """ becomes
    """
      import scala.collection.immutable.Nil.++

      object YYY {

        ++(Nil)

      }
    """
  } applyRefactoring organize

  @Test
  def finalBraceShouldNotBeRemoved() = new FileSet {
    """
      import java.io.Serializable
      object A extends Serializable {

      }""" becomes
      """
      import java.io.Serializable
      object A extends Serializable {

      }"""
  } applyRefactoring organize

  /*
   * See Assembla Ticket #1002506
   */
  @Test
  def localValInClosureShouldNotAddBrokenImports() = new FileSet {
    """
      package com.github.mlangc.experiments

      import com.github.mlangc.experiments.test._

      package test {
        class Test
      }

      class Bug {
        None.getOrElse {
          val x: Test = ???
          x
        }
      }""" becomes
      """
      package com.github.mlangc.experiments

      import com.github.mlangc.experiments.test.Test

      package test {
        class Test
      }

      class Bug {
        None.getOrElse {
          val x: Test = ???
          x
        }
      }"""
  } applyRefactoring organizeWithTypicalParams

  /*
   * See Assembla Ticket #1002166
   */
  @Test
  def dontInsertExtraRoundBrackets1002166() = new FileSet {
    """
      package test
      import scala.collection.mutable.ListBuffer

      object O1

      object O2 {
        ("//")
        ("//")
      }
    """ becomes
    """
      package test

      object O1

      object O2 {
        ("//")
        ("//")
      }
    """
  } applyRefactoring organizeWithTypicalParams

  /*
   * See Assembla Ticket #1002088
   */
  @Test
  def dontInsertExtraRoundBrackets1002088Ex1() = new FileSet {
    """
      package test
      import java.lang.String

      object O1

      trait Bug1 {
        ")"
      }
    """ becomes
    """
      package test

      object O1

      trait Bug1 {
        ")"
      }
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def dontInsertExtraRoundBrackets1002088Ex2() = new FileSet {
    """
      package test
      import java.lang.String

      object Bug2 {
        ")"
      }

      object O2
    """ becomes
    """
      package test

      object Bug2 {
        ")"
      }

      object O2
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def dontInsertExtraRoundBrackets1002088Ex3() = new FileSet {
    """
      package test
      import java.lang.String

      object Bug2 {
        ')'
      }

      object O2
    """ becomes
    """
      package test

      object Bug2 {
        ')'
      }

      object O2
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def dontInsertExtraRoundBrackets1002088Ex4() = new FileSet {
    """
      package test
      import java.lang.String

      object Bug2 {
        val `)` = ')'
      }

      object O2
    """ becomes
    """
      package test

      object Bug2 {
        val `)` = ')'
      }

      object O2
    """
  } applyRefactoring organizeWithTypicalParams

  /*
   * See Assembla Ticket #1002540
   */
  @Test
  def dontFailForCaseClassWithCopy1002540() = new FileSet {
    """
    package test
    import java.net.URL

    case class Bug(i: Int = 1, j: Int = 2) {
      def buggy = copy(j = i)
    }
    """ becomes
    """
    package test

    case class Bug(i: Int = 1, j: Int = 2) {
      def buggy = copy(j = i)
    }
    """
  } applyRefactoring organizeWithTypicalParams

}
