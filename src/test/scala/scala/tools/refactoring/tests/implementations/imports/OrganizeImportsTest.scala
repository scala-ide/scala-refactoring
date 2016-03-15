/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import language.postfixOps
import scala.tools.refactoring.implementations.OrganizeImports.Dependencies

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

  private def organizeWithTypicalParams(pro: FileSet) = organizeCustomized()(pro)

  private def organizeCustomized(
      groupPkgs: List[String] = List("java", "scala", "org", "com"),
      useWildcards: Set[String] = Set("scalaz", "scalaz.Scalaz"),
      dependencies: Dependencies.Value = Dependencies.FullyRecompute)(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = {
      val groupImports = refactoring.GroupImports(groupPkgs)
      val alwaysUseWildcards = refactoring.AlwaysUseWildcards(useWildcards)

      new refactoring.RefactoringParameters(
          options =
            refactoring.ExpandImports ::
            refactoring.PrependScalaPackage ::
            alwaysUseWildcards ::
            refactoring.SortImports ::
            groupImports ::
            Nil,
          deps = dependencies)
    }
  }.mkChanges

  @Test
  def testOrganizeOptions(): Unit = {

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

    new FileSet(expectCompilingCode = false) {
      (src + restOfFile) becomes
      """
      package tests.importing

      import scala.collection.mutable.{HashMap, ListBuffer}
      import scala.io.Source
      import scala.math._
      import scala.math.BigInt
      """ + restOfFile
    } applyRefactoring organizeWithoutCollapsing

    new FileSet(expectCompilingCode = false) {
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

    new FileSet(expectCompilingCode = false) {
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
  def simplifyWildcards() = new FileSet(expectCompilingCode = false) {
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

  /*
   * See Assembla Ticket #1002142
   */
  @Test
  def organizeImportsAndVarargs1002142Ex1() = new FileSet {
    """
    package com.github.mlangc.experiments

    import scala.collection.immutable.HashMap

    object TryOrganizeImportsHere {
      def values: Seq[HashMap[Int, Int]] = ???
      List(values: _*)
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsAndVarargs1002142Ex2() = new FileSet {
    """
    import scala.collection.mutable.HashMap

    object Y {
     def values: List[HashMap[Int, Int]] = ???
    }
    """ isNotModified;
    """
    object X {

      val xs = Y.values
      List(xs: _*)
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  /*
   * See Assembla Ticket #1002526
   */
  @Test
  def organizeImportsDupesImports1002526Ex1() = new FileSet {
    """
    package a.b
    class X
    """ isNotModified;

    """
    package a.c
    class Y
    """ isNotModified;

    """
    /*<-*/
    package a.p

    import a.b.X
    import a.c.Y

    class Test {

      val x: X = ???
      val y: Y = ???
    }
    """ becomes
    """
    /*<-*/
    package a.p

    import a.c.Y

    import a.b.X

    class Test {

      val x: X = ???
      val y: Y = ???
    }
    """;
  } applyRefactoring organizeCustomized(groupPkgs = List("java", "scala", "org", "com", "a.c", "a"))

  @Test
  def organizeImportsDupesImports1002526Ex2() = new FileSet {
    """
    package com.github.mlangc.experiments

    import java.util.Date
    import java.text.SimpleDateFormat
    import java.text.DateFormat
    import java.util.UUID
    import java.io.InputStream
    import java.io.OutputStream

    class Bug {
      def x: (Date, SimpleDateFormat, DateFormat, UUID, InputStream, OutputStream) = ???
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    import java.text.DateFormat
    import java.text.SimpleDateFormat

    import java.util.Date
    import java.util.UUID

    import java.io.InputStream
    import java.io.OutputStream

    class Bug {
      def x: (Date, SimpleDateFormat, DateFormat, UUID, InputStream, OutputStream) = ???
    }
    """
  } applyRefactoring organizeCustomized(groupPkgs = List("java", "java.util", "java.io"))

  @Test
  def organizeImportsWithDuplicateGroups() = new FileSet {
    """
    package test

    import scala.util.Try
    import java.util.Date
    import scala.collection.immutable.Queue

    class Bug {
      def a: (Try[Date], Queue[Date]) = ???
    }
    """ becomes
    """
    package test

    import java.util.Date

    import scala.collection.immutable.Queue
    import scala.util.Try

    class Bug {
      def a: (Try[Date], Queue[Date]) = ???
    }
    """
  } applyRefactoring organizeCustomized(groupPkgs = List("java", "scala", "java"))

  /*
   * See Assembla Ticket 1002613
   */
  @Test
  def organizeImportsRemovesNeededImport1002613Ex1() = new FileSet {
    """
    package test

    import java.util.Collections

    class Bug {
      import Collections.emptyList

      def test = emptyList
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsRemovesNeededImport1002613Ex2() = new FileSet {
    """
    package test

    import java.util.Collections

    class Bug {
      import java.util.Collections.emptyList

      def test = emptyList
    }
    """ becomes
    """
    package test

    class Bug {
      import java.util.Collections.emptyList

      def test = emptyList
    }
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsRemovesNeededImport1002613Ex3() = new FileSet {
    """
    package test

    import java.util.Collections.emptyList

    class Bug {
      import java.util.Collections.emptyList

      def test = emptyList
    }
    """ becomes
    """
    package test

    class Bug {
      import java.util.Collections.emptyList

      def test = emptyList
    }
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsRemovesNeededImport1002613Ex4() = new FileSet {
    """
    package test

    import java.util.Collections

    class Bug {
      def test = {
        import Collections.emptyList
        emptyList
      }
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsSimilarButNotAffectedBy1002613Ex1() = new FileSet {
    """
    package test

    import java.util.Collections.emptyList

    class Bug {
      import java.util.Collections
      import Collections.emptyList

      def test = emptyList
    }
    """ becomes
    """
    package test

    class Bug {
      import java.util.Collections
      import Collections.emptyList

      def test = emptyList
    }
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsWithSimilarImportsAtDifferentScopesEx1() = new FileSet {
    """
    package com.github.mlangc.experiments

    import java.util.Arrays

    class Bug4 {
      import java.util.Collections

      def test1 = Arrays.asList(1, 2)
      def test2 = Collections.emptyList
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsWithSimilarImportsAtDifferentScopesEx2() = new FileSet {
    """
    package com.github.mlangc.experiments

    import java.util.Arrays
    import java.util.Collections

    class Bug4 {
      import java.util.Collections

      def test1 = Arrays.asList(1, 2)
      def test2 = Collections.emptyList
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    import java.util.Arrays

    class Bug4 {
      import java.util.Collections

      def test1 = Arrays.asList(1, 2)
      def test2 = Collections.emptyList
    }
    """
  } applyRefactoring organizeWithTypicalParams

  /*
   * See Assembla Ticket #1002626
   */
  @Test
  def organizeImportsWithRenameClauses1002626Ex1() = new FileSet {
    """
    package com.github.mlangc.experiments

    import java.util.{concurrent => javaConcurrent}
    import scala.{concurrent => scalaConcurrent}

    trait Bug1 {
      def tryOrganizeImportsHere(x: javaConcurrent.atomic.AtomicBoolean): scalaConcurrent.Future[Unit]
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    import java.util.{concurrent => javaConcurrent}

    import scala.{concurrent => scalaConcurrent}

    trait Bug1 {
      def tryOrganizeImportsHere(x: javaConcurrent.atomic.AtomicBoolean): scalaConcurrent.Future[Unit]
    }
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsSimlarTo1002626Ex1ButNoRenameClauses() = new FileSet {
    """
    package com.github.mlangc.experiments

    import java.util
    import scala.concurrent

    trait Bug1 {
      def tryOrganizeImportsHere(x: util.concurrent.atomic.AtomicBoolean): concurrent.Future[Unit]
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    import java.util

    trait Bug1 {
      def tryOrganizeImportsHere(x: util.concurrent.atomic.AtomicBoolean): concurrent.Future[Unit]
    }
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsWithRenameClauses1002626Ex2() = new FileSet {
    """
    package com.github.mlangc.experiments

    import scala.language.implicitConversions
    import java.util.concurrent.atomic.AtomicBoolean
    import java.util.{concurrent => javaConcurrent}
    import scala.{concurrent => scalaConcurrent}

    object Bug2 {
      implicit def tryOrganizeImportsHere[T](scalaFuture: scalaConcurrent.Future[T]): javaConcurrent.Future[T] = ???
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    import java.util.{concurrent => javaConcurrent}

    import scala.{concurrent => scalaConcurrent}
    import scala.language.implicitConversions

    object Bug2 {
      implicit def tryOrganizeImportsHere[T](scalaFuture: scalaConcurrent.Future[T]): javaConcurrent.Future[T] = ???
    }
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsWithRenameClauses1002626Ex3() = new FileSet {
    """
    package com.github.mlangc.experiments

    import scala.language.implicitConversions
    import java.util.concurrent.atomic.AtomicBoolean
    import java.util.{concurrent => javaConcurrent}
    import scala.{concurrent => scalaConcurrent}

    object Bug2 {
      implicit def tryOrganizeImportsHere[T](scalaFuture: scalaConcurrent.Future[T]): javaConcurrent.Future[T] = ???
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    import java.util.{concurrent => javaConcurrent}

    import scala.{concurrent => scalaConcurrent}
    import scala.language.implicitConversions

    object Bug2 {
      implicit def tryOrganizeImportsHere[T](scalaFuture: scalaConcurrent.Future[T]): javaConcurrent.Future[T] = ???
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def organizeImportsWithRenameClauses1002626Ex4() = new FileSet {
    """
    package com.github.mlangc.experiments

    import scala.language.implicitConversions
    import scala.concurrent.{Future => ScalaFuture}
    import java.util.concurrent.{Future => JavaFuture}
    import java.util.concurrent.atomic.AtomicBoolean

    trait Bug3 {
      implicit def tryOrganizeImportsHere[T](javaFuture: JavaFuture[T]): ScalaFuture[T]
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    import java.util.concurrent.{Future => JavaFuture}

    import scala.concurrent.{Future => ScalaFuture}
    import scala.language.implicitConversions

    trait Bug3 {
      implicit def tryOrganizeImportsHere[T](javaFuture: JavaFuture[T]): ScalaFuture[T]
    }
    """
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsWithRenameClauses1002626Ex5() = new FileSet {
    """
    package com.github.mlangc.experiments

    import java.util.concurrent.atomic.{AtomicBoolean => Aboolean, AtomicInteger => Aint, AtomicLong => Along}

    trait Bug4 {
      def tryOrganizeImportsHere(b: Aboolean, i: Aint, l: Along): Double
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    import java.util.concurrent.atomic.{AtomicBoolean => Aboolean}
    import java.util.concurrent.atomic.{AtomicInteger => Aint}
    import java.util.concurrent.atomic.{AtomicLong => Along}

    trait Bug4 {
      def tryOrganizeImportsHere(b: Aboolean, i: Aint, l: Along): Double
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def organizeImportsWithRenameClauses1002626Ex6() = new FileSet {
    """
    package com.github.mlangc.experiments

    import scala.language.implicitConversions

    trait Bug5 {
      implicit def intToString(i: Int) = i.toString
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def importsInDefShouldBeSorted() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.C
        import acme.Acme.{B, A}
        A + B + C
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import acme.Acme.{A, B}
        import fake.Acme.C
        A + B + C
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def importsScatteredInDefShouldBeGatheredAndThenSorted() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.C
        val c = C
        import acme.Acme.{B, A}
        A + B + c
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.C
        val c = C
        import acme.Acme.{A, B}
        A + B + c
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def importsScatteredInDifferentDefsShouldBeProcessedSeparately() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.D
        val d = D
        import acme.Acme.A
        def k = {
          import fake.Acme.C
          import acme.Acme.B
          A + B + C + d
        }
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.D
        val d = D
        import acme.Acme.A
        def k = {
          import acme.Acme.B
          import fake.Acme.C
          A + B + C + d
        }
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def importsShouldNotBeModifiedInVarValLazyValAndLambda() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      var bar = {
        import fake.Acme.D
        val d = D
        import acme.Acme.A
        A + d
      }
      val foo = {
        import fake.Acme.C
        import acme.Acme.B
        B + C + bar
      }
      lazy val baz = {
        import fake.Acme.C
        import acme.Acme.B
        B + C + bar
      }
      def foe = List(1).map { _ =>
        import fake.Acme.C
        import acme.Acme.B
        B + C + bar
      }
    }
    """ isNotModified
    } applyRefactoring organizeWithTypicalParams

  @Test
  def importsScatteredInSameLineOfDefShouldNotBeRearranged() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {import fake.Acme.D; import fake.Acme.C; C + D}
      def k = {
        import fake.Acme.C; import acme.Acme.A
        import acme.Acme.B
        A + B + C
      }
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def unusedImportsInDefShouldBeRemoved() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.D
        val d = D
        import acme.Acme.A
        def k = {
          import fake.Acme.C
          import acme.Acme.B
          B + d
        }
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.D
        val d = D
        def k = {
          import acme.Acme.B
          B + d
        }
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def unusedImportsInDefInImportSelectorsShouldBeRemoved() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.{C, D}
        val d = D
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.D
        val d = D
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def unusedRenamedImportsInDefInImportSelectorsShouldBeRemoved() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.{D => U, C => V}
        import acme.Acme.{A => W}
        val d = U
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.{D => U}
        val d = U
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def importsWithImplicitAsWildcardInDefShouldNotBeRemovedBecauseAreUsed() = new FileSet {
    """
    package acme

    class A(i: Int) {
      def foo(implicit b: Int) = i + b
    }

    object A {
      implicit def intToA(i: Int): A = new A(i)
      implicit val B = 6
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import acme.A._
        5.foo
      }
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def importsWithImplicitInDefShouldBeSorted() = new FileSet {
    """
    package acme

    class A(i: Int) {
      def foo(implicit b: Int) = i + b
    }

    object A {
      implicit def intToA(i: Int): A = new A(i)
      implicit val B = 6
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import acme.A.intToA
        import acme.A.B
        5.foo
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import acme.A.B
        import acme.A.intToA
        5.foo
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def duplicateImportsInDefShouldBeRemoved() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme.D
        import acme.Acme.{A, D}
        import fake.Acme.D
        val d = D
        def k = {
          import fake.Acme.{C, B, B}
          import acme.Acme.B
          A + B + C + d
        }
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import acme.Acme.A
        import fake.Acme.D
        val d = D
        def k = {
          import acme.Acme.B
          import fake.Acme.C
          A + B + C + d
        }
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def wildcardDuplicateImportsInDefShouldBeRemoved() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import fake.Acme._
        import acme.Acme.{A, D}
        import fake.Acme.D
        import fake.Acme.{C, B, B}
        import acme.Acme.B
        val d = D
        A + B + C + d
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import acme.Acme.A
        import acme.Acme.B
        import fake.Acme._
        val d = D
        A + B + C + d
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def wildcardDuplicateImportsInSelectorInDefShouldBeRemoved() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val C = 7
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import acme.Acme.B
        import acme.Acme.{A, _}
        import fake.Acme.D
        val d = D
        A + B + d
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      def foo = {
        import acme.Acme._
        import fake.Acme.D
        val d = D
        A + B + d
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInExistentialTypeOfMethodDeclarationAndNotRemoveItFromImports() = new FileSet {
    """
    /*<-*/
    package test

    import scala.util.Either
    import scala.util.Try

    trait A {
      def foo: (Try[Unit], _)
    }
    """ becomes {
    """
    /*<-*/
    package test

    import scala.util.Try

    trait A {
      def foo: (Try[Unit], _)
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInExistentialTypeOfClassDeclarationAndNotRemoveItFromImports() = new FileSet {
    """
    /*<-*/
    package test

    import scala.util.Either
    import scala.util.Try

    class TestTE(val a: (Try[_], _))
    """ becomes {
    """
    /*<-*/
    package test

    import scala.util.Try

    class TestTE(val a: (Try[_], _))
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInInnerExistentialTypeOfClassDeclarationAndNotRemoveItFromImports() = new FileSet {
    """
    /*<-*/
    package test

    import scala.util.Either
    import scala.util.Try

    class TestTE(val a: (Try[_], Int))
    """ becomes {
    """
    /*<-*/
    package test

    import scala.util.Try

    class TestTE(val a: (Try[_], Int))
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInExistentialTypeOfHigherKindedTypeAndNotRemoveItFromImports() = new FileSet {
    """
    /*<-*/
    package test

    import scala.util.Either
    import scala.util.Try

    trait Test[Try, B]
    class ParamCheck[A]
    class Verify extends ParamCheck[Test[_, Either[_, _]]]
    """ becomes {
    """
    /*<-*/
    package test

    import scala.util.Either

    trait Test[Try, B]
    class ParamCheck[A]
    class Verify extends ParamCheck[Test[_, Either[_, _]]]
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInExistentialTypeOfDeepInHigherKindedTypeAndNotRemoveItFromImports() = new FileSet {
    """
    /*<-*/
    package test

    import scala.util.Either
    import scala.util.Try

    trait Test[Try, B]
    class ParamCheck[A]
    class Verify extends ParamCheck[Test[_, Either[_, Try[_]]]]
    """ becomes {
    """
    /*<-*/
    package test

    import scala.util.Either
    import scala.util.Try

    trait Test[Try, B]
    class ParamCheck[A]
    class Verify extends ParamCheck[Test[_, Either[_, Try[_]]]]
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotDiscoverArgTypeInExistentialTypeOfClassDeclarationAndRemoveItFromImports() = new FileSet {
    """
    /*<-*/
    package test

    import scala.util.Either
    import scala.util.Try

    class TestTE(val a: (_, _))
    """ becomes {
    """
    /*<-*/
    package test

    class TestTE(val a: (_, _))
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotThrowAnExceptionForComplexExpressionsInMethodBody() = new FileSet {
    """
    /*<-*/
    package test

    object X {
      val m = Map[String, String]()

      def f = {
        import scala.util.Try
        for ((a, b) ← m)
          println((a,b))
        0
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    object X {
      val m = Map[String, String]()

      def f = {
        for ((a, b) ← m)
          println((a,b))
        0
      }
    }
    """
    }
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotThrowAnExceptionWhenImportsAreCollapsedToWildcardImport() = new FileSet {
    """
    package a.b
    class C1
    class C2
    """ isNotModified

    """
    /*<-*/
    package d.e

    import a.b.C1
    import a.b.C2

    object X {
      val c1 = new C1
      val c2 = new C2
    }
    """ becomes
    """
    /*<-*/
    package d.e

    import a.b._

    object X {
      val c1 = new C1
      val c2 = new C2
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify, useWildcards = Set("a.b"))

  @Test
  def shouldNotOrganizeImportsForDetachedImportsInDefBlock() = new FileSet {
    """
    package acme

    class Acme(val A: Int) {
      val B = 6
    }

    object AcmeHelper {
      val H = 7
    }
    """ isNotModified

    """
    package org

    class Acne(val A: Int)
    """ isNotModified

    """
    /*<-*/
    package test

    class A {
      def foo = {
        import org.Acne
        import acme._

        val tested = new Acme((new Acne(4)).A)
        import tested._

        import acme.AcmeHelper.H

        B + H
    }
    """ becomes {
    """
    /*<-*/
    package test

    class A {
      def foo = {
        import acme._
        import org.Acne

        val tested = new Acme((new Acne(4)).A)
        import acme.AcmeHelper.H
        import tested._

        B + H
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldOrganizeImportsForRelativeImport() = new FileSet {
    """
    package acme

    class Acme {
      val B = 6
    }

    object AcmeHelper {
      val H = 7
    }
    """ isNotModified

    """
    package acme.abefore

    class Abscess(val A: Int)
    """ isNotModified

    """
    /*<-*/
    package test

    class A {
      def foo = {
        import acme._
        import abefore._
        import acme.AcmeHelper.H

        val tested = new Acme
        import tested._

        B + H + (new Abscess(3)).A
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class A {
      def foo = {
        import acme._
        import acme.AcmeHelper.H
        import abefore._

        val tested = new Acme
        import tested._

        B + H + (new Abscess(3)).A
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotOrganizeImportsForDetachedImportsInNestedDefBlockWhenImportIsInOuterToo() = new FileSet {
    """
    package acme

    class Acme(val A: Int) {
      val B = 6
    }

    class AcmeHelper {
      val H = 7
    }
    """ isNotModified

    """
    package org

    class Acne(val A: Int)
    """ isNotModified

    """
    /*<-*/
    package test

    class A {
      def foo = {
        import acme.AcmeHelper

        def bar = {
          import org.Acne
          import acme.Acme

          val tested = new Acme((new Acne(4)).A)
          import tested._

          val help = new AcmeHelper
          import help._

          object inner {
            val I = 5
          }

          import inner.I

          A + H + I
        }
        bar
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class A {
      def foo = {
        import acme.AcmeHelper

        def bar = {
          import acme.Acme
          import org.Acne

          val tested = new Acme((new Acne(4)).A)
          import tested._

          val help = new AcmeHelper
          import help._

          object inner {
            val I = 5
          }

          import inner.I

          A + H + I
        }
        bar
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldOrganizeImportsForOuterAndInnerDefBlocks() = new FileSet {
    """
    package acme

    class Acme(val A: Int) {
      val B = 6
    }

    class AcmeHelper {
      val H = 7
    }
    """ isNotModified

    """
    package org

    class Acne(val A: Int)
    class AcneHelper(val AH: Int)
    """ isNotModified

    """
    /*<-*/
    package test

    class A {
      def foo = {
        import org.AcneHelper
        import acme.AcmeHelper

        def bar = {
          import org.Acne
          import acme.Acme

          val tested = new Acme((new Acne(4)).A)
          import tested._

          val help = new AcmeHelper
          import help._

          object inner {
            val I = 5
          }

          import inner.I

          A + H + I + (new AcneHelper(5)).AH
        }
        bar
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class A {
      def foo = {
        import acme.AcmeHelper
        import org.AcneHelper

        def bar = {
          import acme.Acme
          import org.Acne

          val tested = new Acme((new Acne(4)).A)
          import tested._

          val help = new AcmeHelper
          import help._

          object inner {
            val I = 5
          }

          import inner.I

          A + H + I + (new AcneHelper(5)).AH
        }
        bar
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotDisplaceDetachedImport() = new FileSet {
    """
    package acme

    class Map
    """ isNotModified

    """
    /*<-*/
    package test

    class A {
      def foo = {
        val scalaMap = Map[Int, Int]()
        import acme.Map
        val acmeMap: Map = new Map
        acmeMap
      }
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotDisplaceDetachedImportForParameterizedType() = new FileSet {
    """
    package acme

    class Map[K, V]
    """ isNotModified

    """
    /*<-*/
    package test

    class A {
      def foo = {
        val scalaMap = Map[Int, Int]()
        import acme.Map
        val acmeMap: Map[Int, String] = new Map[Int, String]
        acmeMap
      }
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotDisplaceDetachedShadowingImport() = new FileSet {
    """
    /*<-*/
    package test

    class A {
      def foo = {
        val scalaMap = Map[Int, Int]()
        import java.util.Map
        import java.util.HashMap
        val javaMap: Map[Int, Int] = new HashMap[Int, Int]()
        scalaMap
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class A {
      def foo = {
        val scalaMap = Map[Int, Int]()
        import java.util.HashMap
        import java.util.Map
        val javaMap: Map[Int, Int] = new HashMap[Int, Int]()
        scalaMap
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldSortImportsInClassBody() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.B
      import fake.Acme.D
      import acme.Acme.A

      def foo = {
        val d = D
        A + B + d
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.A
      import acme.Acme.B
      import fake.Acme.D

      def foo = {
        val d = D
        A + B + d
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveImportInObjectBodyForNonWhitespaceImportNeighborhood_v1() = new FileSet {
    """
    package acme

    object Acme { import java.util.Map }
    """ becomes {
    """
    package acme

    object Acme {  }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveImportInObjectBodyForNonWhitespaceImportNeighborhood_v2() = new FileSet {
    """
    package acme

    object Acme {
      import java.util.Map
    }
    """ becomes {
    """
    package acme

    object Acme {
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveImportInObjectBodyForNonWhitespaceImportNeighborhood_v3() = new FileSet {
    """
    package acme

    object Acme { import java.util.Map
    }
    """ becomes {
    """
    package acme

    object Acme {     }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveImportInObjectBodyForNonWhitespaceImportNeighborhood_v4() = new FileSet {
    """
    package acme

    object Acme {
      import java.util.Map }
    """ becomes {
    """
    package acme

    object Acme {
 }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldSortImportsInClassBodyAndInNestedObject() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.B
      import fake.Acme.D
      import acme.Acme.A

      object InnerBar {
        import java.util.Map
        import java.util.HashMap

        val a: Map[String, String] = new HashMap
      }

      def foo = {
        val d = D
        A + B + d
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.A
      import acme.Acme.B
      import fake.Acme.D

      object InnerBar {
        import java.util.HashMap
        import java.util.Map

        val a: Map[String, String] = new HashMap
      }

      def foo = {
        val d = D
        A + B + d
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveDuplicatedImportsInClassAndObjectBodies() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    object Acme {
      val D = 11
    }
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.{ B, _ }
      import fake.Acme.D
      import acme.Acme.A

      object InnerBar {
        import java.util._
        import java.util.HashMap

        val a: Map[String, String] = new HashMap
      }

      def foo = {
        val d = D
        A + B + d
      }
    }
    """ becomes {
    """
    /*<-*/
    package test

    class Bar {
      import acme.Acme._
      import fake.Acme.D

      object InnerBar {
        import java.util._

        val a: Map[String, String] = new HashMap
      }

      def foo = {
        val d = D
        A + B + d
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  /** There is a bug in package scope imports organizing. Import Map => JavaMap should not be moved there. */
  @Test
  def shouldNotRemoveRenamedImportInClassBody() = new FileSet {
    """
    /*<-*/
    package test

    class Bar {
      import java.util.{Map => JavaMap}
      import java.util.HashMap

      val a: JavaMap[String, String] = new HashMap
    }
    """ becomes {
    """
    /*<-*/
    package test

    import java.util.{Map => JavaMap}

    class Bar {
      import java.util.HashMap
      import java.util.{Map => JavaMap}

      val a: JavaMap[String, String] = new HashMap
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  /** There is a bug in package scope imports organizing. Import ListBuffer should not be moved there. */
  @Test
  def shouldOrganizeImportInClassAndDefBodies() = new FileSet {
    """
    /*<-*/
    package test

    class Bar {
      import java.util.Map
      import java.util.HashMap

      def foo: List[String] = {
        import scala.collection.mutable.ListBuffer
        import scala.collection.mutable.Buffer

        Buffer
        ListBuffer[String]().toList
      }

      val a: Map[String, String] = new HashMap
    }
    """ becomes {
    """
    /*<-*/
    package test

    import scala.collection.mutable.ListBuffer

    class Bar {
      import java.util.HashMap
      import java.util.Map

      def foo: List[String] = {
        import scala.collection.mutable.Buffer
        import scala.collection.mutable.ListBuffer

        Buffer
        ListBuffer[String]().toList
      }

      val a: Map[String, String] = new HashMap
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreventLocalObjectImportInMethod() = new FileSet {
    """
    /*<-*/
    package test

    object AnObject {
      val a = 3
    }

    object Test {
      def test(): Int = {
        import AnObject._
        a
      }
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams
}
