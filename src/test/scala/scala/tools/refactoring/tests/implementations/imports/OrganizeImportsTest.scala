/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import scala.language.postfixOps
import scala.tools.refactoring.implementations.OrganizeImports
import scala.tools.refactoring.implementations.OrganizeImports.Dependencies
class OrganizeImportsTest extends OrganizeImportsBaseTest {
  private def organize(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val oiConfig = OrganizeImports.OrganizeImportsConfig(importsStrategy = Some(OrganizeImports.ImportsStrategy.CollapseImports))
    val params = new RefactoringParameters(config = Some(oiConfig))
  }.mkChanges

  private def organizeWithoutCollapsing(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    import refactoring.oiWorker.participants._
    val oiConfig = OrganizeImports.OrganizeImportsConfig(None)
    val params = new RefactoringParameters(options = List(SortImportSelectors), config = Some(oiConfig))
  }.mkChanges

  private def organizeExpand(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val oiConfig = OrganizeImports.OrganizeImportsConfig(importsStrategy = Some(OrganizeImports.ImportsStrategy.ExpandImports))
    val params = new refactoring.RefactoringParameters(options = Nil,
      config = Some(oiConfig))
  }.mkChanges

  private def organizeWithTypicalParams(pro: FileSet) = organizeCustomized()(pro)

  private def organizeCustomized(
    groupPkgs: List[String] = List("java", "scala", "org", "com"),
    useWildcards: Set[String] = Set("scalaz", "scalaz.Scalaz"),
    dependencies: Dependencies.Value = Dependencies.FullyRecompute,
    organizeLocalImports: Boolean = true)(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val oiConfig = OrganizeImports.OrganizeImportsConfig(
      importsStrategy = Some(OrganizeImports.ImportsStrategy.ExpandImports),
      wildcards = useWildcards,
      groups = groupPkgs)
    import refactoring.oiWorker.participants._
    val params = {
      new refactoring.RefactoringParameters(
        options =
            PrependScalaPackage ::
            Nil,
        deps = dependencies,
        organizeLocalImports = organizeLocalImports,
        config = Some(oiConfig))
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

      object Main {val lb = ListBuffer(1); val hm = HashMap(1 → 1) }
    """ becomes
      """
      package tests.importing

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      object Main {val lb = ListBuffer(1); val hm = HashMap(1 → 1) }
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

      object Main {val lb = ListBuffer(1); val hm = HashMap(1 → 1) }
    """ becomes
      """
      package tests.importing

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      object Main {val lb = ListBuffer(1); val hm = HashMap(1 → 1) }
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

      object Main {val s: S = ""; var o: Objekt = null}
    """ becomes
      """
      import java.lang.{Object => Objekt, String => S}

      object Main {val s: S = ""; var o: Objekt = null}
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
  def unusedImportWildcards_v1() = new FileSet {
    """
      import java.util._
      import scala.collection._

      object Main {
      }    """ becomes
      """

      object Main {
      }    """
  } applyRefactoring organize

  @Test
  def unusedImportWildcards_v2() = new FileSet {
    """
      import java.util._
      import scala.collection._
      ▒
      object Main {
      }    """.replace("▒", "") becomes
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
    """ becomes {
      """
    package test

    class Bug {
      import java.util.Collections.emptyList

      def test = emptyList
    }
    """
    }
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

    import java.util.Collections.emptyList

    class Bug {

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
    """ becomes {
      """
    package test

    class Bug {
      def test = {
        import java.util.Collections.emptyList
        emptyList
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def organizeImportsSimilarButNotAffectedBy1002613Ex1() = new FileSet {
    """
    package test

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
      import java.util.Collections.emptyList

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
    import java.util.Collections

    class Bug4 {

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

  /** Potentially remove one more empty line between package and class */
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
        import acme.abefore._

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

  @Test
  def shouldNotRemoveRenamedImportInClassBody() = new FileSet {
    """
    /*<-*/
    package test

    class Bar {
      import java.util.HashMap
      import java.util.{Map => JavaMap}

      val a: JavaMap[String, String] = new HashMap
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

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

  @Test
  def shouldRemoveDuplicatedImportFromDef() = new FileSet {
    """
    /*<-*/
    package test

    object AnObject {
      val a = 3
    }

    object Test {
      import AnObject._
      def test(): Int = {
        import AnObject._
        a
      }
    }
    """ becomes {
      """
    /*<-*/
    package test

    object AnObject {
      val a = 3
    }

    object Test {
      import AnObject._
      def test(): Int = {
        a
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveDuplicatedImportsInScopes_v1() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    class Acme {
      val D = 11
    }

    class Ecma(val E: Int)
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.B
      import java.util.Map

      object InnerBar {
        import acme.Acme.A
        import acme.Acme.B
        import fake.Ecma
        import java.util.HashMap

        val a: Map[String, String] = new HashMap

        val ecma = new Ecma(4)
        import ecma.E

        val e = E
        def foo = {
          import acme.Acme.A
          import acme.Acme.B
          A + B
        }
      }

      import fake.Ecma
      def foo = {
        import fake.Acme
        import fake.Ecma

        val newD = new Acme
        import acme.Acme.A
        import acme.Acme.B
        import newD.D
        A + B + D + (new Ecma(4)).E
      }
    }
    """ becomes {
      """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.B
      import java.util.Map

      object InnerBar {
        import acme.Acme.A
        import fake.Ecma
        import java.util.HashMap

        val a: Map[String, String] = new HashMap

        val ecma = new Ecma(4)
        import ecma.E

        val e = E
        def foo = {
          A + B
        }
      }

      import fake.Ecma
      def foo = {
        import fake.Acme

        val newD = new Acme
        import acme.Acme.A
        import newD.D
        A + B + D + (new Ecma(4)).E
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveDuplicatedImportsInScopes_v2() = new FileSet {
    """
    package acme

    object Acme {
      val A = 5
      val B = 6
    }
    """ isNotModified

    """
    package fake

    class Acme {
      val D = 11
    }

    class Ecma(val E: Int)
    """ isNotModified

    """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.B
      import java.util.Map

      def foo = {
        import acme.Acme.A
        def bar = {
          import acme.Acme.A
          A
        }
        A + B + bar
      }
      import fake.Acme
      object InnerBar {
        import fake.Acme
        def foo = {
          import fake.Acme
          val a = new Acme
          def bar = {
            import fake.Ecma
            import java.util.HashMap
            import java.util.Map
            val a: Map[Int, Int] = new HashMap
            a.size + (new Ecma(4)).E
          }
          a.D + bar
        }
      }
    }
    """ becomes {
      """
    /*<-*/
    package test

    class Bar {
      import acme.Acme.B
      import java.util.Map

      def foo = {
        import acme.Acme.A
        def bar = {
          A
        }
        A + B + bar
      }
      import fake.Acme
      object InnerBar {
        def foo = {
          val a = new Acme
          def bar = {
            import fake.Ecma
            import java.util.HashMap
            val a: Map[Int, Int] = new HashMap
            a.size + (new Ecma(4)).E
          }
          a.D + bar
        }
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveDuplicatedImportFromDefWithComment() = new FileSet {
    """
    /*<-*/
    package test

    object AnObject {
      val a = 3
      val b = 4
    }

    object Test {
      import AnObject.b
      def test(): Int = {
        import AnObject.a
        // comment for b
        import AnObject.b
        a + b
      }
    }
    """ becomes {
      """
    /*<-*/
    package test

    object AnObject {
      val a = 3
      val b = 4
    }

    object Test {
      import AnObject.b
      def test(): Int = {
        import AnObject.a
        a + b
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveDuplicatedImportFromDefWithComment_v2() = new FileSet {
    """
    /*<-*/
    package test

    object AnObject {
      val a = 3
      val b = 4
    }

    object Test {
      import AnObject.b
      def test(): Int = {
        // comment for a
        import AnObject.a
        // comment for b
        import AnObject.b
        b
      }
    }
    """ becomes {
      """
    /*<-*/
    package test

    object AnObject {
      val a = 3
      val b = 4
    }

    object Test {
      import AnObject.b
      def test(): Int = {
        b
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveDuplicatedImportFromDefWithComment_v3() = new FileSet {
    """
    /*<-*/
    package test

    object AnObject {
      val a = 3
      val b = 4
    }

    object Test {
      import AnObject.b
      def test(): Int = {
        import AnObject.a
        /**
          * comment for b
          */
        import AnObject.b
        a + b
      }
    }
    """ becomes {
      """
    /*<-*/
    package test

    object AnObject {
      val a = 3
      val b = 4
    }

    object Test {
      import AnObject.b
      def test(): Int = {
        import AnObject.a
        a + b
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotRemoveComments_variation1() = new FileSet {
    """
    /*<-*/
    package test

    trait Bug {
      import scala.concurrent.Future
      // HELP
      import scala.util.Try
      import java.util.ArrayList

      val l: ArrayList[Int]
      val t: Try[Int]
      val f: Future[Double]
    }
    """ becomes {
      """
    /*<-*/
    package test

    trait Bug {
      import java.util.ArrayList
      import scala.concurrent.Future
      // HELP
      import scala.util.Try

      val l: ArrayList[Int]
      val t: Try[Int]
      val f: Future[Double]
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotRemoveCommentsInImportPackagePrefix() = new FileSet {
    """
    /*<-*/
    package test

    trait Bug {
      import scala./*comment*/util/*comment.comment*/.Try
      import scala/*comment.comment*/.concurrent.Future
      import java.util./*comment.*/Date
      import /*comment.comment*/java.util.List
      import java.util/*.comment*/.ArrayList

      val d: Date
      val al: ArrayList[Int]
      val t: Try[Int]
      val f: Future[Double]
      val l: List[String]
    }
    """ becomes {
      """
    /*<-*/
    package test

    trait Bug {
      import java.util/*.comment*/.ArrayList
      import java.util./*comment.*/Date
      import /*comment.comment*/java.util.List
      import scala/*comment.comment*/.concurrent.Future
      import scala./*comment*/util/*comment.comment*/.Try

      val d: Date
      val al: ArrayList[Int]
      val t: Try[Int]
      val f: Future[Double]
      val l: List[String]
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldCorrectlyRenderBacktickedStableIdentifier() = new FileSet {
    """
    /*<-*/
    package test

    object arrow {
      val `=>` = 42
    }

    object dollar {
      val `$$$` = 24
    }

    trait Bug {
      import dollar.`$$$`
      import arrow.`=>`

      val dollarWithArrow = `$$$` + `=>`
    }
    """ becomes {
      """
    /*<-*/
    package test

    object arrow {
      val `=>` = 42
    }

    object dollar {
      val `$$$` = 24
    }

    trait Bug {
      import arrow.`=>`
      import dollar.`$$$`

      val dollarWithArrow = `$$$` + `=>`
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotRemoveComments_variation2() = new FileSet {
    """
    /*<-*/
    package test

    trait Bug {
      /**
       * belongs to Future
       */
      import scala.concurrent.Future
      // belongs to Try
      import scala.util.Try

      // belongs to ArrayList
      import java.util.ArrayList
      /* belongs to Date */
      import java.util.Date

      val l: ArrayList[Int]
      val d: Date
      val t: Try[Int]
      val f: Future[Double]
    }
    """ becomes {
      """
    /*<-*/
    package test

    trait Bug {
      // belongs to ArrayList
      import java.util.ArrayList
      /* belongs to Date */
      import java.util.Date
      /**
       * belongs to Future
       */
      import scala.concurrent.Future
      // belongs to Try
      import scala.util.Try

      val l: ArrayList[Int]
      val d: Date
      val t: Try[Int]
      val f: Future[Double]
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotRemoveComments_variation3() = new FileSet {
    """
    /*<-*/
    package test

    trait Bug {
      /* comment about trait innards
       */
      // belongs to Try
      import scala.util.Try
      /**
       * belongs to Future
       */
      import scala.concurrent.Future

      val t: Try[Int]
      val f: Future[Double]
    }
    """ becomes {
      """
    /*<-*/
    package test

    trait Bug {
      /* comment about trait innards
       */
      /**
       * belongs to Future
       */
      import scala.concurrent.Future
      // belongs to Try
      import scala.util.Try

      val t: Try[Int]
      val f: Future[Double]
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotRemoveComments_variation4() = new FileSet {
    """
    /*<-*/
    package test

    trait Bug {
      /* comment about trait innards
       */

      import scala.util.Try
      /**
       * belongs to Future
       */
      import scala.concurrent.Future

      val t: Try[Int]
      val f: Future[Double]
    }
    """ becomes {
      """
    /*<-*/
    package test

    trait Bug {
      /* comment about trait innards
       */

      /**
       * belongs to Future
       */
      import scala.concurrent.Future
      import scala.util.Try

      val t: Try[Int]
      val f: Future[Double]
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveBacktickedNamesOfImports_variation1() = new FileSet {
    """
    package com.github.mlangc.experiments

    object Bug7 {
      object ` => ` {
        val x = 42
      }
    }

    class Bug7 {
      import Bug7.` => `
      val x = ` => `.x
    }
  """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveBacktickedNamesOfImports_variation2() = new FileSet {
    """
    package com.github.mlangc.experiments

    trait Bug5 {
      import scala.util.{Try => `Evil....`}
      import scala.concurrent.Future

      def f: Future[`Evil....`[Int]] = ???
    }""" becomes {
      """
    package com.github.mlangc.experiments

    trait Bug5 {
      import scala.concurrent.Future
      import scala.util.{Try => `Evil....`}

      def f: Future[`Evil....`[Int]] = ???
    }"""
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveBacktickedNamesOfImports_variation3() = new FileSet {
    """
    package com.github.mlangc.experiments

    object Bug4 {
      class `€€€` {
        def x = 42
      }

      class DollarDollarDollar {
        def y = 53
      }
    }

    class Bug4 {
      import Bug4.`€€€`
      import Bug4.{DollarDollarDollar => `$$$`}

      new `€€€`().x
      new `$$$`().y
    }""" becomes {
      """
    package com.github.mlangc.experiments

    object Bug4 {
      class `€€€` {
        def x = 42
      }

      class DollarDollarDollar {
        def y = 53
      }
    }

    class Bug4 {
      import Bug4.{DollarDollarDollar => `$$$`}
      import Bug4.`€€€`

      new `€€€`().x
      new `$$$`().y
    }"""
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveCommentInImportAndSortImports() = new FileSet {
    """
    package com.github.mlangc.experiments

    import scala.concurrent.Future

    object Bug3 {
      class Other {
        type Tpe1 = Int
        type Tpe2 = Double
        type Tpe3 = String
      }
    }

    trait Bug3 {
      import scala.util./*evil.evil*/Try
      import Bug3._
      import scala.concurrent.Future

      val other: Other

      def f: Future[Int] = {
        import other.{ Tpe1 => Dpe0 }
        import other.Tpe2

        def z: Dpe0 = {
          val x: Dpe0 = 12
          val y: Tpe2 = 0.0
          x + y.toInt
        }

        Future.successful(Try(z).getOrElse(42))
      }
    }""" becomes {
      """
    package com.github.mlangc.experiments

    import scala.concurrent.Future

    object Bug3 {
      class Other {
        type Tpe1 = Int
        type Tpe2 = Double
        type Tpe3 = String
      }
    }

    trait Bug3 {
      import Bug3._
      import scala.util./*evil.evil*/Try

      val other: Other

      def f: Future[Int] = {
        import other.{Tpe1 => Dpe0}
        import other.Tpe2

        def z: Dpe0 = {
          val x: Dpe0 = 12
          val y: Tpe2 = 0.0
          x + y.toInt
        }

        Future.successful(Try(z).getOrElse(42))
      }
    }"""
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveUnusedImportOfClassDefinedInSameEnclosingPackage() = new FileSet {
    """
    package com.github.mlangc.experiments

    package bugs {
      sealed trait MyList[+T]
      case class ::[T](head: T, tail: MyList[T]) extends MyList[T]
      case object MyNil extends MyList[Nothing]
    }

    class Bug6 {
      def f1 = {
        import bugs.::
        import bugs.MyList
        import bugs.MyNil

        new ::(1, MyNil)
      }
    }""" becomes {
      """
    package com.github.mlangc.experiments

    package bugs {
      sealed trait MyList[+T]
      case class ::[T](head: T, tail: MyList[T]) extends MyList[T]
      case object MyNil extends MyList[Nothing]
    }

    class Bug6 {
      def f1 = {
        import com.github.mlangc.experiments.bugs.::
        import com.github.mlangc.experiments.bugs.MyNil

        new ::(1, MyNil)
      }
    }"""
    }
  } applyRefactoring organizeWithTypicalParams

  /**
   * This case is quite peculiar. `scala.language` is imported always and moved to top most package scope.
   *  See at following snippet in [[scala.tools.refactoring.analysis.CompilationUnitDependencies]] class:
   *  {{{
   *  // Always add the SIP 18 language imports as required until we can handle them properly
   *  case Import(select @ Select(Ident(nme.scala_), `language`), feature) =>
   *    feature foreach (selector => addToResult(Select(select, selector.name)))
   *  }}}
   */
  @Test
  def shouldNotRemoveImportWithUnusedImplicitsBecauseItComesFromScalaLanguagePackage_v1() = new FileSet {
    """
    package com.github.mlangc.experiments

    class Bug8 {
      import scala.language.implicitConversions
      implicit def intToString(i: Int) = i.toString
    }""" isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotRemoveImportWithUnusedImplicitsBecauseItComesFromScalaLanguagePackage_v2() = new FileSet {
    """
    package com.github.mlangc.experiments

    import scala.language.implicitConversions

    class Bug8 {
      import scala.language.implicitConversions
      implicit def intToString(i: Int) = i.toString
    }""" becomes
      """
    package com.github.mlangc.experiments

    import scala.language.implicitConversions

    class Bug8 {
      implicit def intToString(i: Int) = i.toString
    }"""
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldRemoveImportWithUnusedImplicits() = new FileSet {
    """
    package com.github.mlangc.experiments

    class Bug8 {
      import scala.concurrent.duration.DurationConversions
      implicit def intToString(i: Int) = i.toString
    }""" becomes {
      """
    package com.github.mlangc.experiments

    class Bug8 {
      implicit def intToString(i: Int) = i.toString
    }"""
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInExistentialTypeOfMethodDeclarationAndNotRemoveItFromImportsInClassScope() = new FileSet {
    """
    /*<-*/
    package test

    trait A {
      import scala.util.Either
      import scala.util.Try
      def foo: (Try[Unit], _)
    }
    """ becomes {
      """
    /*<-*/
    package test

    trait A {
      import scala.util.Try
      def foo: (Try[Unit], _)
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInExistentialTypeOfClassDeclarationAndNotRemoveItFromImportsInClassScope() = new FileSet {
    """
    /*<-*/
    package test

    trait Outer {
      import scala.util.Either
      import scala.util.Try
      abstract class TestTE(val a: (Try[_], _))
    }
    """ becomes {
      """
    /*<-*/
    package test

    trait Outer {
      import scala.util.Try
      abstract class TestTE(val a: (Try[_], _))
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInInnerExistentialTypeOfClassDeclarationAndNotRemoveItFromImportsInClassScope() = new FileSet {
    """
    /*<-*/
    package test

    object Outer {
      import scala.util.Either
      import scala.util.Try

      class TestTE(val a: (Try[_], Int))
    }
    """ becomes {
      """
    /*<-*/
    package test

    object Outer {
      import scala.util.Try

      class TestTE(val a: (Try[_], Int))
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInExistentialTypeOfHigherKindedTypeAndNotRemoveItFromImportsInClassScope() = new FileSet {
    """
    /*<-*/
    package test

    trait Outer {
      import scala.util.Either
      import scala.util.Try

      trait Test[Try, B]
      class ParamCheck[A]
      class Verify extends ParamCheck[Test[_, Either[_, _]]]
    }
    """ becomes {
      """
    /*<-*/
    package test

    trait Outer {
      import scala.util.Either

      trait Test[Try, B]
      class ParamCheck[A]
      class Verify extends ParamCheck[Test[_, Either[_, _]]]
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldDiscoverArgTypeInExistentialTypeOfDeepInHigherKindedTypeAndNotRemoveItFromImportsInClassScope() = new FileSet {
    """
    /*<-*/
    package test

    object Outer {
      import scala.util.Either
      import scala.util.Try

      trait Test[Try, B]
      class ParamCheck[A]
      class Verify extends ParamCheck[Test[_, Either[_, Try[_]]]]
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotDiscoverArgTypeInExistentialTypeOfClassDeclarationAndRemoveItFromImportsInClassScope() = new FileSet {
    """
    /*<-*/
    package test

    object Outer {
      import scala.util.Either
      import scala.util.Try

      class TestTE(val a: (_, _))
    }
    """ becomes {
      """
    /*<-*/
    package test

    object Outer {

      class TestTE(val a: (_, _))
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveImportsForCompoundTypesInClassScope() = new FileSet {
    """
    package test

    object Outer {
      import java.util.SortedMap
      import java.util.concurrent.ConcurrentMap

      type A[K, V] = SortedMap[K, V] with ConcurrentMap[K, V]
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldNotOrganizeLocalImportsInMethod() = new FileSet {
    """
    package test

    object Acme {
      val A: Int = 5
      val B: Int = 6
    }

    class Tested {
      import Acme.B
      def foo = {
        import Acme.{ B, A, _ }
        A + B
      }
    }

    object TestedObj {
      import Acme.A
    }

    trait TestedTrait {
      import Acme._
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(organizeLocalImports = false)

  @Test
  def shouldPreserveRenamedTypesInImportInClass() = new FileSet {
    """
    package test

    trait Tested {
      import java.util.{ Map => JavaMap, `List` => JavaList, ArrayList, _ }
      def foo: JavaMap[String, String]
      def bar: JavaList[Int] = new ArrayList[Int]
    }
    """ becomes {
      """
    package test

    trait Tested {
      import java.util.{`List` => JavaList, Map => JavaMap, _}
      def foo: JavaMap[String, String]
      def bar: JavaList[Int] = new ArrayList[Int]
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveRenamedTypesInImportInMethod() = new FileSet {
    """
    package test

    trait Tested {
      def foo = {
        import java.util.{ Map => JavaMap, ArrayList, _ }
        def bar: JavaMap[String, String] = new HashMap[String, String]
        def baz: ArrayList[Int] = new ArrayList[Int]
        bar
        baz
      }
    }
    """ becomes {
      """
    package test

    trait Tested {
      def foo = {
        import java.util.{Map => JavaMap, _}
        def bar: JavaMap[String, String] = new HashMap[String, String]
        def baz: ArrayList[Int] = new ArrayList[Int]
        bar
        baz
      }
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveBackticksInPackagePath_v1() = new FileSet {
    """
    object `Eye[` {
      object `.Of` {
        object Sauron
        object TheBeholder
      }
    }

    class Bug {
      import `Eye[`.`.Of`.TheBeholder
      import `Eye[`.`.Of`.Sauron

      val x = Sauron
      val y = TheBeholder
    }
    """ becomes {
      """
    object `Eye[` {
      object `.Of` {
        object Sauron
        object TheBeholder
      }
    }

    class Bug {
      import `Eye[`.`.Of`.Sauron
      import `Eye[`.`.Of`.TheBeholder

      val x = Sauron
      val y = TheBeholder
    }
    """
    }
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveSpacesInImportInMethod() = new FileSet {
    """
    package test

    trait Tested {
      def fooImportWith2Spaces = {
        import  java.util.Map
        def bar: Map[String, String] = ???
        bar
      }
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveImportOfTypeOfImplicitVal() = new FileSet {
    """
    package test

    import scala.collection.mutable.ListBuffer

    class Test(implicit lb: ListBuffer[Int]) {
      def f = {
        import scala.collection.mutable.Buffer
        Buffer(1)
      }
    }""" isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveFormattingForImportInPatMat() = new FileSet {
    """
    package test

    class Test {
      def f = {
        1 match {
          case 1 =>
            import scala.collection.mutable.Buffer
            Buffer(1)
        }
      }
    }""" isNotModified
  } applyRefactoring organizeWithTypicalParams

  @Test
  def shouldPreserveOriginalFormattingInImportSelectors() = new FileSet {
    """
      import java.lang.{Integer =>I}
      import java.lang.{String => S}

      object Main {
        val s: S = ""
        val l: I = 1
      }
    """ becomes
      """
      import java.lang.{Integer =>I, String => S}

      object Main {
        val s: S = ""
        val l: I = 1
      }
    """
  } applyRefactoring organize

  @Test
  def shouldNotRemoveUsedImports_v1() = new FileSet {
    """
    import scala.concurrent.duration
    import scala.concurrent.duration.FiniteDuration

    object Main extends App {
      val d1: FiniteDuration = ???
      val d2: duration.FiniteDuration = ???
    }
    """ isNotModified
  } applyRefactoring organize

  @Test
  def shouldNotRemoveUsedImports_v2() = new FileSet {
    """
    object Test1 {
      import scala.language.implicitConversions
      trait Foo
      implicit def toFoo(s: String): Foo = ???
      val f: Foo = "hi"
    }

    object Test2 {
      import scala.language.dynamics
      trait Foo extends scala.Dynamic
    }
    """ isNotModified
  } applyRefactoring organize

  @Test
  def shouldRemoveDupilcatedImports() = new FileSet {
    """
    package test

    import java.util.List

    trait A {
      import java.util.List
      val a: List[String] = ???

      def foo = {
        import java.util.List
        val a: List[Int] = ???
        a
      }

      import java.util.Map
      trait AA {
        import java.util.Map
        val m: Map[String, Int] = ???

        def bar = {
          import java.util.Map
          val m: Map[Int, Int] = ???
          m
        }

        import scala.util.Try
        def baz = {
          import scala.util.Try
          val b: Try[Int] = ???
          b
        }

        def far = {
          import scala.util.Random
          def faz = {
            import scala.util.Random
            val r: Random = ???
            r
          }
        }
      }
    }
    """ becomes
      """
    package test

    import java.util.List

    trait A {
      val a: List[String] = ???

      def foo = {
        val a: List[Int] = ???
        a
      }

      import java.util.Map
      trait AA {
        val m: Map[String, Int] = ???

        def bar = {
          val m: Map[Int, Int] = ???
          m
        }

        import scala.util.Try
        def baz = {
          val b: Try[Int] = ???
          b
        }

        def far = {
          import scala.util.Random
          def faz = {
            val r: Random = ???
            r
          }
        }
      }
    }
    """
  } applyRefactoring organize

  @Test
  def shouldNotRemoveImportWithImplicit() = new FileSet {
    """
    package test
    import scala.collection.JavaConverters.asScalaBufferConverter

    trait A {
      import java.util.ArrayList
      val a = (new ArrayList[Int]).asScala.toList
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldRemoveUnusedImportWithImplicit() = new FileSet {
    """
    package test
    import scala.collection.JavaConverters.asScalaBufferConverter

    trait A {
      import java.util.ArrayList
      val a = new ArrayList[Int]
    }
    """ becomes
      """
    package test
    trait A {
      import java.util.ArrayList
      val a = new ArrayList[Int]
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  /**
   * In this test we should rather expect that `import b.B` in package scope will be removed.
   * It is not because of this piece of CompilationUnitDepenedencies:
   * {{{
   *         case imp @ Import(qualifier, selector)  => {
   *           if (inScopeForLocalImports) {
   *             if (!qualifier.symbol.isLocal && !qualifier.symbol.isVal) {
   *               if (isRelativeToTopLevelImports(qualifier) || isRelativeToEnclosingPackage(qualifier)) {
   *                 fakeSelectTree(qualifier.tpe, qualifier.symbol, qualifier) match {
   *                   case select: Select => addToResult(select)
   *                   case _ => ()
   *                 }
   *               }
   *             }
   *             localImports ::= imp
   *           } else {
   *             topLevelImports ::= imp
   *           }
   *         }
   * }}}
   *
   * In future the `isRelativeToTopLevelImports(qualifier)` will be removed from this condition.
   */
  @Test
  def shouldNotRemoveUnusedImportExtendedByWildcard_part1() = new FileSet {
    """
    package b

    object B {
      def foo = "B"
    }
    """ isNotModified

    """
    /*<-*/
    package test
    import b.B

    trait A {
      import B._
      val a = foo
    }
    """ becomes
      """
    /*<-*/
    package test
    trait A {
      import b.B._
      val a = foo
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveUnusedImportExtendedByWildcard_part2() = new FileSet {
    """
    package b

    object B {
      def foo = "B"
    }
    """ isNotModified

    """
    /*<-*/
    package test
    import b.B

    trait A {
      import b.B._
      val a = foo
    }
    """ becomes
    """
    /*<-*/
    package test
    trait A {
      import b.B._
      val a = foo
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportExtendedByWildcard_v1() = new FileSet {
    """
    package b

    object B {
      def foo = "B"
    }
    """ isNotModified

    """
    /*<-*/
    package test
    import b.B

    trait A {
      import b.B._
      val a = foo
    }

    object C {
      def bar = {
        B.foo
      }
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportExtendedByWildcard_v2() = new FileSet {
    """
    package b

    object B {
      def foo = "B"
    }
    """ isNotModified

    """
    /*<-*/
    package test
    import b.B

    object A {
      import b.B._
      val a = foo
      def bar = {
        B.foo
      }
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenTypeDefWith_v1() = new FileSet {
    """
    package b

    trait A[T]
    trait B[T]
    """ isNotModified

    """
    /*<-*/
    package testa
    import b.A
    import b.B

    object Tested {
      val aWithBParametrized: A[Int] with B[String] = ???
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenTypeDefWith_v2() = new FileSet {
    """
    package bb

    trait C
    trait D
    """ isNotModified

    """
    /*<-*/
    package test
    import bb.C
    import bb.D

    object Tested {
      val cWithD: C with D = ???
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenExistentialType() = new FileSet {
    """
    package bb

    trait E[T]
    trait F
    """ isNotModified

    """
    /*<-*/
    package test
    import bb.E
    import bb.F

    class Tested {
      val eExistential: E[_ <: F] = ???
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotUnfoldValIdentifier() = new FileSet {
  """
  package bc
  trait AA {
    object InAA {
      def foo = 5
    }
  }
  """ isNotModified

  """
  /*<-*/
  package test
  import bc.AA
  class Tested(context: AA) {
    import context._
    import context.InAA.foo

    def bar = foo
  }
  """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  // this test produces compilation error in 2.12, because shadowing an implicit parameter is not possible
  @ScalaVersion(doesNotMatch = "2.12")
  def shouldNotRemoveImportImplicit() = new FileSet {
  """
  /*<-*/
  package test

  import scala.concurrent.ExecutionContext
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  object Tested {
    import scala.concurrent.ExecutionContext.Implicits.global
    def foo() = {
      Future.successful(5)
    }
  }

  class Tested {
    private def bar(f: =>Future[Int])(implicit ec: ExecutionContext): Unit = {
      f.onComplete { f => println("done"); }
    }

    def baz() = bar {
      Future.successful(5)
    }
  }
  """ becomes
  """
  /*<-*/
  package test

  import scala.concurrent.ExecutionContext
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  object Tested {
    def foo() = {
      Future.successful(5)
    }
  }

  class Tested {
    private def bar(f: =>Future[Int])(implicit ec: ExecutionContext): Unit = {
      f.onComplete { f => println("done"); }
    }

    def baz() = bar {
      Future.successful(5)
    }
  }
  """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  /** Problem found in Play for
   *  play.api.i18n.Messages.apply("text")(play.api.i18n.Messages.Implicits.applicationMessages(i18n.this.Lang.defaultLang, play.api.Play.current))
   */
  def shouldNotRemoveDeeplyAppliedImplicit() = new FileSet {
    """
    package testing

    object Messages {
      object Implicits {
        implicit def applicationMessages(implicit lang: String, application: Long): Messages =
          new Messages(lang, application)
      }

      def apply(text: String)(implicit messages: Messages) = messages
    }

    class Messages(val lang: String, val application: Long)
    """ isNotModified

    """
    package testing.acme

    object Implicits {
      implicit val application: Long = 0L
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import testing.Messages
    import testing.Messages.Implicits._
    import testing.acme.Implicits.application

    class Tested {
      implicit val defaultLang: String = "IT"
      def foo = Messages("text")
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenExtendedClassHasExplicitTypeParam() = new FileSet {
    """
    package acme

    class Extended[T](val a: T, t: String)
    """ isNotModified

    """
    /*<-*/
    package acme.test

    import acme.Extended

    class Tested(val id: Int) extends Extended[Int](id, "text")
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportIfInPathToInnerType() = new FileSet {
    """
    package acme

    trait Tupler[T]

    object Tupler {
      type Aux[T] = Tupler[T]
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import acme.Tupler

    trait Tested {
      def foo[T](implicit aux: Tupler.Aux[T]): Unit
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportIfInPathToInheritedInnerType() = new FileSet {
    """
    package acme

    trait Tupler[T]

    trait TuplerInstance {
      type Aux[T] = Tupler[T]
    }

    object Tupler extends TuplerInstance
    """ isNotModified

    """
    /*<-*/
    package tested

    import acme.Tupler

    trait Tested {
      def foo[T](implicit aux: Tupler.Aux[T]): Unit
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportIfTypeInHigherKindedType() = new FileSet {
    """
    package acme

    object tag {
      type @@[L, R] = L with R
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import acme.tag.@@

    trait Selector[T]

    trait Tested {
      def foo[T, U](implicit sel: Selector[T @@ U]): Unit
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportLanguageExperimental_v1() = new FileSet {
    """
    /*<-*/
    package tested

    import scala.language.experimental

    trait Tested
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportLanguageExperimental_v2() = new FileSet {
    """
    /*<-*/
    package tested

    import scala.language.experimental._

    trait Tested
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportLanguageExperimentalMacros() = new FileSet {
    """
    /*<-*/
    package tested

    import scala.language.experimental.macros

    trait Tested
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenJustPackage_v1() = new FileSet {
    """
    package acme.whitebox

    trait Context
    """ isNotModified

    """
    /*<-*/
    package tested

    import acme.whitebox

    class Tested(val c: whitebox.Context)
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenJustPackage_v2() = new FileSet {
    """
    package macros.whitebox

    trait Context
    """ isNotModified

    """
    /*<-*/
    package tested

    import macros.whitebox

    class Tested(val c: whitebox.Context)
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenJustPackageInner_v1() = new FileSet {
    """
    package acme.whitebox

    trait Context
    """ isNotModified

    """
    /*<-*/
    package tested

    object runtime {
      import acme.whitebox
      class Tested(val c: whitebox.Context)
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenJustPackageInner_v2() = new FileSet {
    """
    package acme
    package whitebox

    trait Context
    """ isNotModified

    """
    /*<-*/
    package acme
    package tested

    object runtime {
      import whitebox.Context
      class Tested(val c: whitebox.Context)
    }
    """ becomes
    """
    /*<-*/
    package acme
    package tested

    object runtime {
      import acme.whitebox.Context
      class Tested(val c: whitebox.Context)
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenImportFromDeepValue() = new FileSet {
    """
    package macros.whitebox

    abstract class UniverseApi {
      def reify[T](tree: T): Unit
    }

    abstract class Universe extends UniverseApi {
      type Tree
    }

    trait Context {
      val universe: Universe
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import macros.whitebox

    class Tested(val c: whitebox.Context) {
      import c.universe._

      def foo(tree: Tree) = ???
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenMethodRenamed_v1() = new FileSet {
    """
    /*<-*/
    package tested

    import java.lang.Integer.{rotateLeft => rotl}

    class Tested {
      val t = rotl(5, 5)
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenMethodRenamed_v2() = new FileSet {
    """
    /*<-*/
    package tested

    class Tested {
      import java.lang.Integer.{rotateLeft => rotl}
      val t = rotl(5, 5)
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenPackageRenamedAndUsedInClassOf_v1() = new FileSet {
    """
    /*<-*/
    package tested

    import java.{lang => jlang}

    class Tested {
      val t = classOf[jlang.Integer]
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenPackageRenamedAndUsedInClassOf_v2() = new FileSet {
    """
    /*<-*/
    package tested

    class Tested {
      import java.{lang => jlang}
      val t = classOf[jlang.Integer]
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenPackageRenamedAndUsedInClassOf_v3() = new FileSet {
    """
    package org.acme

    class Integer
    """ isNotModified

    """
    /*<-*/
    package tested

    import org.{acme => jlang}

    class Tested {
      val t = classOf[jlang.Integer]
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenPackageRenamedAndUsedInClassOf_v4() = new FileSet {
    """
    package org.acme

    class Integer
    """ isNotModified

    """
    /*<-*/
    package tested

    class Tested {
      import org.{acme => jlang}
      val t = classOf[jlang.Integer]
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenPackageRenamed_v1() = new FileSet {
    """
    package org.acme

    class Integer
    """ isNotModified

    """
    /*<-*/
    package tested

    import org.{acme => jlang}

    class Tested {
      val t = new jlang.Integer
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenPackageRenamed_v2() = new FileSet {
    """
    package org.acme

    class Integer
    """ isNotModified

    """
    /*<-*/
    package tested

    class Tested {
      import org.{acme => jlang}
      val t = new jlang.Integer
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenJavaPackageRenamed_v1() = new FileSet {
    """
    /*<-*/
    package tested

    import java.{lang => jlang}

    class Tested {
      val t = new jlang.Integer(42)
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenJavaPackageRenamed_v2() = new FileSet {
    """
    /*<-*/
    package tested

    class Tested {
      import java.{lang => jlang}
      val t = new jlang.Integer(42)
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportWhenTypeInCake() = new FileSet {
    """
    package testruntime

    trait Names {
      type TermName

      val TermName: TermNameExtractor

      abstract class TermNameExtractor {
        def apply(s: String): TermName
        def unapply(name: TermName): Option[String]
      }
    }

    class NamesImpl extends Names {
      type TermName = String
      val TermName = new TermNameExtractorString
      class TermNameExtractorString extends TermNameExtractor {
        def apply(s: String) = s
        def unapply(name: TermName): Option[String] = Some(name)
      }
    }

    object universe {
      lazy val space: Names = new NamesImpl
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import testruntime.universe.space.TermName

    class Tested {
      val t: TermName = ???
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportExtendedByWildcard_v3() = new FileSet {
    """
    package acme.duration

    trait DurationConvensions {
      def baz = "baz"
    }

    object Duration {
      def foo = "Duration"
    }
    """ isNotModified

    """
    package acme

    package object duration {
      implicit class bar(b: String) extends DurationConvensions
    }
    """ isNotModified

    """
    package acme

    class Promise[T]
    """ isNotModified

    """
    /*<-*/
    package test
    import acme.Promise
    import acme.duration.Duration

    object A {
      def f = {
        import acme.duration._
        val p = new Promise[Int]
        println(p)
        "5".baz
      }
      def g = Duration.foo
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportFromSamePackageWhenTypeUsedInImplicit_relativePath() = new FileSet {
    """
    package api

    trait TypeTags {
      trait TypeTag[T]
      def typeOf[T](implicit tt: TypeTag[T]) = ???
    """ isNotModified

    """
    package tested

    object inner {
      type Inner[T, U] = T with U
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import api.TypeTags

    class Test extends TypeTags {
      import inner.Inner

      implicit val innerTT = new TypeTag[Inner[_, _]] {}

      val test = typeOf[Inner[_, _]]
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportFromSamePackageWhenTypeUsedInImplicit_absolutePath() = new FileSet {
    """
    package api

    trait TypeTags {
      trait TypeTag[T]
      def typeOf[T](implicit tt: TypeTag[T]) = ???
    """ isNotModified

    """
    package tested

    object inner {
      type Inner[T, U] = T with U
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import api.TypeTags

    class Test extends TypeTags {
      import tested.inner.Inner

      implicit val innerTT = new TypeTag[Inner[_, _]] {}

      val test = typeOf[Inner[_, _]]
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotRemoveImportFromSamePackageWhenInnerTraitUsed() = new FileSet {
    """
    package tested

    object inner {
      type Inner = Int
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import inner.Inner

    class Test {
      val test: Inner = ???
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Ignore("FIXME")
  @Test
  def shouldNotRemoveImportWhenTypeParametrizedByObjectInConstructor() = new FileSet {
    """
    package org.acme

    trait Inner {
      type Inner
    }

    case class Toolbox[T <: Inner](inner: T)
    """ isNotModified

    """
    package org.acne

    import org.acme.Inner

    object inner extends Inner {
      type Inner = Int
    }
    """ isNotModified

    """
    /*<-*/
    package tested

    import org.acme.Toolbox
    import org.acne.inner

    case class Test(test: Toolbox[inner.type]) {
      import test.inner._

      val a: Inner = 5
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Ignore("FIXME")
  @Test
  def shouldRemoveUnusedWildcardImport() = new FileSet {
    """
    package tested

    import java.util._
    import java.util.concurrent._
    import java.util.concurrent.atomic._

    trait Bug {
      def tryOrganizeImportsHere: AtomicLong
    }
    """ becomes
    """
    package tested

    import java.util.concurrent.atomic._

    trait Bug {
      def tryOrganizeImportsHere: AtomicLong
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotShrinkImportsToWildcardIfRenamingUsed_v1() = new FileSet {
    """
    package akka.actor

    trait ActorRef
    trait ActorSystem
    """ isNotModified

    """
    /*<-*/
    package com.example.actors

    import My._
    import akka.actor.{ActorRef => _, _}

    object Test {
      val sys: ActorSystem = ???
      val ref: ActorRef = ???
    }

    object My {
      trait ActorRef {
        def foo: Int
      }
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotShrinkImportsToWildcardIfRenamingUsed_v2() = new FileSet {
    """
    package akka.actor

    trait ActorRef
    trait ActorSystem
    """ isNotModified

    """
    /*<-*/
    package com.example.actors

    import My._
    import akka.actor.{ActorRef => AkkaActorRef, _}

    object Test {
      val sys: ActorSystem = ???
      val ref: ActorRef = ???
      val aref: AkkaActorRef = ???
    }

    object My {
      trait ActorRef {
        def foo: Int
      }
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotShrinkImportsToWildcardIfRenamingUsed_v3() = new FileSet {
    """
    package akka.actor

    trait ActorRef
    trait ActorSystem
    """ isNotModified

    """
    /*<-*/
    package com.example.actors

    import my._
    import akka.actor.{ActorRef => _, _}

    object Test {
      val sys: ActorSystem = ???
      val ref: ActorRef = ???
    }

    object my {
      trait ActorRef {
        def foo: Int
      }
    }
    """ becomes
    """
    /*<-*/
    package com.example.actors

    import akka.actor.{ActorRef => _, _}
    import my._

    object Test {
      val sys: ActorSystem = ???
      val ref: ActorRef = ???
    }

    object my {
      trait ActorRef {
        def foo: Int
      }
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotShrinkImportsToWildcardIfRenamingUsed_v4() = new FileSet {
    """
    package akka.actor

    trait ActorRef
    trait ActorSystem
    """ isNotModified

    """
    /*<-*/
    package com.example.actors

    object Test {
      import My._
      import akka.actor.{ActorRef => _, _}

      val sys: ActorSystem = ???
      val ref: ActorRef = ???
    }

    object My {
      trait ActorRef {
        def foo: Int
      }
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotShrinkImportsToWildcardIfRenamingUsed_v5() = new FileSet {
    """
    package akka.actor

    trait ActorRef
    trait ActorSystem
    """ isNotModified

    """
    /*<-*/
    package com.example.actors

    object Test {
      import My._
      import akka.actor.{ActorRef => AkkaActorRef, _}

      val sys: ActorSystem = ???
      val ref: ActorRef = ???
      val aref: AkkaActorRef = ???
    }

    object My {
      trait ActorRef {
        def foo: Int
      }
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotShrinkImportsToWildcardIfRenamingUsed_v6() = new FileSet {
    """
    package akka.actor

    trait ActorRef
    trait ActorSystem
    """ isNotModified

    """
    /*<-*/
    package com.example.actors

    object Test {
      import my._
      import akka.actor.{ActorRef => _, _}

      val sys: ActorSystem = ???
      val ref: ActorRef = ???
    }

    object my {
      trait ActorRef {
        def foo: Int
      }
    }
    """ becomes
    """
    /*<-*/
    package com.example.actors

    object Test {
      import akka.actor.{ActorRef => _, _}
      import my._

      val sys: ActorSystem = ???
      val ref: ActorRef = ???
    }

    object my {
      trait ActorRef {
        def foo: Int
      }
    }
    """
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotShrinkImportsToWildcardIfRenamingUsed_v7() = new FileSet {
    """
    package akka.actor

    trait ActorRef
    trait ActorSystem
    """ isNotModified

    """
    /*<-*/
    package com.example.actors

    object Test {
      import My._
      import akka.actor.{ActorRef => AkkaActorRef}

      val ref: ActorRef = ???
      val aref: AkkaActorRef = ???
    }

    object My {
      trait ActorRef {
        def foo: Int
      }
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)

  @Test
  def shouldNotShrinkImportsToWildcardIfRenamingUsed_v8() = new FileSet {
    """
    package akka.actor

    trait ActorRef
    trait ActorSystem
    """ isNotModified

    """
    /*<-*/
    package com.example.actors

    import My._
    import akka.actor.{ActorRef => AkkaActorRef}

    object Test {
      val ref: ActorRef = ???
      val aref: AkkaActorRef = ???
    }

    object My {
      trait ActorRef {
        def foo: Int
      }
    }
    """ isNotModified
  } applyRefactoring organizeCustomized(dependencies = Dependencies.RecomputeAndModify)
}
