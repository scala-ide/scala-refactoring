package scala.tools.refactoring
package implementations.oimports

import scala.annotation.tailrec
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global

class TreeToolbox[G <: Global](val global: G) {
  import scala.collection._
  import global._

  class TreeCollector[T <: Tree] private (traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) extends Traverser {
    private val collected_ = mutable.ListBuffer.empty[(T, Symbol)]
    def collect(tree: T): Unit = collected_ += (tree -> currentOwner)
    def collected = collected_.toList
    override def traverse(tree: Tree): Unit = traverserBody(this).orElse[Tree, Unit] {
      case t => super.traverse(t)
    }(tree)
  }

  private object TreeCollector {
    def apply[T <: Tree](traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]) = new TreeCollector[T](traverserBody)
  }

  def forTreesOfKind[T <: Tree](tree: Tree)(traverserBody: TreeCollector[T] => PartialFunction[Tree, Unit]): List[(T, Symbol)] = {
    val treeTraverser = TreeCollector[T](traverserBody)
    treeTraverser.traverse(tree)
    treeTraverser.collected
  }

  object removeScopesDuplicates {
    private def isAncestorOf(kid: Region, elder: Region): Boolean = {
      val kidOwner = kid.owner
      val elderOwner = elder.owner
      kidOwner.ownerChain.contains(elderOwner)
    }

    def apply(regions: List[Region]): List[Region] = {
      regions.sortBy {
        _.startPos.start
      }.map { kid =>
        val ancestors = regions.filter { potentialAncestor =>
          potentialAncestor.startPos.start < kid.startPos.start && isAncestorOf(kid, potentialAncestor) }
        val ancestorsImports = ancestors.flatMap { ancestor =>
          ancestor.imports.map { ancestor.printImport }
        }
        kid.copy(imports = kid.imports.collect {
          case imp if !ancestorsImports.contains(kid.printImport(imp)) => imp
        })
      }
    }
  }

  import global.syntaxAnalyzer._
  class CommentScanner(source: SourceFile) extends SourceFileScanner(source) {
    private val comments_ = mutable.ListBuffer[RangePosition]()
    override def skipComment(): Boolean = {
      val start = this.offset
      val result = super.skipComment()
      if (result) {
        comments_ += new RangePosition(source, start, start, this.charOffset)
      }
      result
    }

    def scan(): Unit = {
      init()
      import scala.tools.nsc.ast.parser.Tokens.EOF
      @tailrec def scan(): Unit = if (token == EOF) () else { nextToken(); scan() }
      scan()
    }

    def comments = comments_.toList
  }
}
