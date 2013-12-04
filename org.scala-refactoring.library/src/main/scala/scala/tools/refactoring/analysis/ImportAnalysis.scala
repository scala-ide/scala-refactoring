package scala.tools.refactoring.analysis

import scala.tools.refactoring.common.CompilerAccess

trait ImportAnalysis extends CompilerAccess {
  import global._

  trait ImportTree extends Traversable[ImportTree]{
    val enclosing: Tree
    
    val children: List[ImportTree]

    val pos: Position

    def isImportedAt(sym: Symbol, refPos: Position): Boolean =
      pos.includes(refPos) && {
        imports(sym) || children.exists(_.isImportedAt(sym, refPos))
      }

    def imports(sym: Symbol): Boolean

    def impStringFor(s: Symbol, imp: Import, name: Name) = {
      val parts = s.effectiveOwner.ownerChain.collect {
        case s if !s.isConstructor && !s.isRoot && !s.isPackageObject =>
          s.nameString
      }
      (name.decode :: parts).reverse.mkString(".")
    }

    def childrenToString =
      children.reverse.mkString("{", ", ", "}")
      
    def foreach[U](f: ImportTree => U) = {
      f(this)
      children.foreach(_.foreach(f))
    }
  }

  case class Root(enclosing: Tree, children: List[ImportTree]) extends ImportTree {
    val pos = 
      children.foldRight[Position](NoPosition)((it, acc) => it.pos union acc)

    def imports(sym: Symbol) = 
      sym.isInDefaultNamespace

    override def toString = 
      childrenToString
  }

  case class ExplicitImport(imp: Import, selector: ImportSelector, enclosing: Tree, children: List[ImportTree]) extends ImportTree {
    val pos =
      imp.pos.withEnd(enclosing.pos.end)

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
      imp.pos.withEnd(enclosing.pos.end)

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

  object ImportTree {
    def build(root: Tree): ImportTree =
      Root(root, build(root, root.children))

    private def build(enclosing: Tree, ts: List[Tree]): List[ImportTree] = ts match {
      case Nil => Nil
      case t :: rest => t match {
        case imp: Import =>
          importToImportTrees(enclosing, imp, build(enclosing, rest))
        case t =>
          build(enclosing, rest) ::: build(t, t.children)
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
}