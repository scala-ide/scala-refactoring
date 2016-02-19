package scala.tools.refactoring.analysis

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.common.EnrichedTrees
import scala.tools.refactoring.transformation.TreeTransformations

trait ImportAnalysis extends TreeFactory with EnrichedTrees with TreeTransformations with CompilerAccess {
  import global._

  /**
   * Import trees represents a set of import statements and allows
   * queries to determine if a symbol is imported at a certain position.
   *
   * Import trees are constructed with `ImportTree.build()`.
   */
  sealed trait ImportTree extends Traversable[ImportTree] {
    /**
     * An AST tree that encloses the according import statement.
     */
    val enclosing: Tree

    /**
     * Import trees that come after this import.
     */
    val children: List[ImportTree]

    /**
     * The range where symbols imported by this import are visible.
     */
    val pos: Position

    val importStatement: Option[Import]

    /**
     * Is symbol `sym` imported by this tree or a subtree at position `refPos`?
     *
     * ```
     * object O{
     *   import hello.World
     *   // World is imported
     * }
     * // World is not imported
     * ```
     */
    def isImportedAt(sym: Symbol, refPos: Position): Boolean =
      sym != NoSymbol && pos.includes(refPos) && {
        imports(sym) || children.exists(_.isImportedAt(sym, refPos))
      }

    /**
     * Is symbol `sym` imported by this import?
     */
    def imports(sym: Symbol): Boolean

    /**
     * Returns an import statement, that imports `sym` at position `refPos`.
     * Returns `None` if no import is found.
     */
    def findImportStatementFor(sym: Symbol, refPos: Position): Option[Import] =
      if (sym != NoSymbol && pos.includes(refPos))
        if (imports(sym))
          importStatement
        else
          children.collectFirst {
            case it if it.isImportedAt(sym, refPos) => it.findImportStatementFor(sym, refPos)
          }.getOrElse(None)
      else
        None

    /**
     * Searches in `t` for all symbols that are not imported if the tree would be
     * inserted at `refPos` and returns a list of the according imports statements.
     */
    def findRequiredImports(t: Tree, originPos: Position, newPos: Position): List[Import] = {
      val symbolsWithImports = t.collect {
        case st: SymTree => (st.symbol, findImportStatementFor(st.symbol, originPos))
      }.collect {
        case (sym, Some(imp)) => (sym, imp)
      }

      symbolsWithImports.filter {
        case (sym, imp) => !isImportedAt(sym, newPos)
      }.map(_._2).distinct
    }

    def childrenToString =
      children.reverse.mkString("{", ", ", "}")

    /**
     * Enables breadth-first traversing of ImportTrees.
     */
    def foreach[U](f: ImportTree => U) = {
      f(this)
      children.foreach(_.foreach(f))
    }
  }

  case class Root(enclosing: Tree, chs: List[ImportTree]) extends ImportTree {

    val children = {
      val predefImp = WildcardImport(mkImportFromStrings("scala.Predef", "_"), enclosing, chs)
      val scalaPkgImp = WildcardImport(mkImportFromStrings("scala.`package`", "_"), enclosing, predefImp :: Nil)
      val listImpTree = mkImportFromStrings("scala.collection.immutable", "List")
      ExplicitImport(listImpTree, listImpTree.selectors(0), enclosing, scalaPkgImp :: Nil)
    } :: Nil

    val pos =
      children.foldRight[Position](NoPosition)((it, acc) => it.pos union acc)

    val importStatement = None

    def imports(sym: Symbol) =
      false

    override def toString =
      childrenToString
  }

  case class ExplicitImport(imp: Import, selector: ImportSelector, enclosing: Tree, children: List[ImportTree]) extends ImportTree {
    val pos =
      if (imp.pos == NoPosition)
        enclosing.pos
      else
        imp.pos.withEnd(enclosing.pos.end)

    val importStatement = Some(imp)

    val importString =
      imp.expr.toString() + "." + selector.name.decode

    def imports(sym: Symbol) = {
      val parts = sym.ownerChain.collect {
        case s if !s.isConstructor && !s.isRoot && !s.isPackageObjectOrClass =>
          s.nameString
      }
      val symImpStr = parts.reverse.mkString(".")

      symImpStr == importString
    }

    override def toString =
      importString + childrenToString
  }

  case class WildcardImport(imp: Import, enclosing: Tree, children: List[ImportTree]) extends ImportTree {
    val pos =
      if (imp.pos == NoPosition)
        enclosing.pos
      else
        imp.pos.withEnd(enclosing.pos.end)

    val importStatement = Some(imp)

    val importString =
      imp.expr.toString() + "." + nme.WILDCARD.decoded

    def imports(sym: Symbol) = {
      val parts = sym.effectiveOwner.ownerChain.collect {
        case s if !s.isConstructor && !s.isRoot && !s.isPackageObject =>
          s.nameString
      }
      val symImpStr = (nme.WILDCARD.decoded :: parts).reverse.mkString(".")

      symImpStr == importString
    }

    override def toString =
      importString + childrenToString
  }

  /**
   * Creates an ImportTree over all import statements in `root`.
   */
  def buildImportTree(root: Tree): ImportTree =
    Root(root, buildImportTree(root, root.children))

  private def buildImportTree(enclosing: Tree, ts: List[Tree]): List[ImportTree] = ts match {
    case Nil => Nil
    case t :: rest => t match {
      case imp: Import =>
        importToImportTrees(enclosing, imp, buildImportTree(enclosing, rest))
      case t =>
        buildImportTree(enclosing, rest) ::: buildImportTree(t, t.children)
    }
  }

  private def importToImportTrees(enclosing: Tree, imp: Import, children: List[ImportTree]): List[ImportTree] = {
    imp.selectors.foldRight(children) { (sel, children) =>
      if (sel.name == nme.WILDCARD)
        WildcardImport(imp, enclosing, children) :: Nil
      else
        ExplicitImport(imp, sel, enclosing, children) :: Nil
    }
  }
}
