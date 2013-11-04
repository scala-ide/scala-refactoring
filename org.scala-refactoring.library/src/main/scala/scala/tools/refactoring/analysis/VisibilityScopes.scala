package scala.tools.refactoring.analysis

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.Selections
import scala.reflect.internal.Flags
import PartialFunction._

trait VisibilityScopes { self: CompilerAccess with Selections =>
  import global._

  /**
   * A data structure that represents all visible symbols at a given position.
   *
   * ```
   * class A(cp: Int){
   *   def a = 1
   *   def b(p: Int) = {
   *     val x = 2
   *     val y = 3
   *     /*(*/p * x * y/*)*/ // <-- reference selection
   *   }
   *   def c = 4
   * }
   * ```
   *
   * ```
   * reference selection -sees->
   *   Block(value y) -sees->
   *     Block(value x) -sees->
   *       DefDef(value p) -sees->
   *        Template(constructor A, value a, value b, value c) -sees->
   *          PackageDef(class A)
   * ```
   */
  trait VisibilityScope {
    /** All declarations in this visibility scope */
    val declarations: List[DefTree] = collectDeclarations

    /** The visibility scopes visible from this scope */
    val visibleScopes: List[VisibilityScope]

    /** The tree that encloses this visibility scope */
    val enclosing: Tree

    /** All symbols declared in this visibility scope */
    lazy val symbols =
      for (decl <- declarations) yield decl.symbol

    /** All symbols that are visible from this visibility scope */
    lazy val allVisibleSymbols: List[Symbol] =
      symbols ::: visibleScopes.flatMap(_.allVisibleSymbols)

    /** Position that includes all declarations in this visibility scope */
    lazy val pos: Position =
      declarations.foldRight[Position](NoPosition) { (t, pos) =>
        pos union t.pos
      }

    override def toString = {
      getClass().getSimpleName() + symbols.mkString("(", ", ", ")") + visibleScopes.mkString("{ ", ", ", " }")
    }

    private[analysis] def exclusionFlags =
      Flags.ACCESSOR

    private[analysis] def collectDeclarations =
      enclosing.children.collect {
        case dt: DefTree if !dt.symbol.hasFlag(exclusionFlags) && dt.symbol != NoSymbol =>
          dt
      }
  }

  class PackageScope(
    val enclosing: PackageDef,
    val visibleScopes: List[VisibilityScope]) extends VisibilityScope

  class TemplateScope(
    val enclosing: Template,
    val visibleScopes: List[VisibilityScope]) extends VisibilityScope

  class MethodScope(
    val enclosing: DefDef,
    val visibleScopes: List[VisibilityScope]) extends VisibilityScope

  class FunctionScope(
    val enclosing: Function,
    val visibleScopes: List[VisibilityScope]) extends VisibilityScope

  class BlockScope(
    val enclosing: Block,
    val visibleScopes: List[VisibilityScope],
    override val declarations: List[DefTree]) extends VisibilityScope

  object VisibilityScope {
    def apply(s: Selection) = {
      val enclosingTrees = {
        val traverser = new FilterTreeTraverser(cond(_) {
          case t => t.pos.includes(s.pos) && !t.pos.sameRange(s.pos)
        })
        traverser.traverse(s.root)
        traverser.hits.toList.reverse
      }

      def childrenFromBlock(block: Block, childTrees: List[VisibilityScope]): VisibilityScope = {
        def inner(stats: List[Tree]): List[VisibilityScope] = stats match {
          case (t: DefTree) :: rest if t.pos.point <= s.pos.point =>
            new BlockScope(block, inner(rest), t :: Nil) :: Nil
          case _ :: rest => inner(rest)
          case Nil => childTrees
        }

        inner((block.stats ::: block.expr :: Nil).reverse).head
      }

      def children(enclosingTrees: List[Tree]): List[VisibilityScope] =
        enclosingTrees match {
          case t :: rest => t match {
            case t: PackageDef => new PackageScope(t, children(rest)) :: Nil
            case t: Template => new TemplateScope(t, children(rest)) :: Nil
            case t: DefDef => new MethodScope(t, children(rest)) :: Nil
            case t: Function => new FunctionScope(t, children(rest)) :: Nil
            case t: Block => childrenFromBlock(t, children(rest)) :: Nil
            case _ => children(rest)
          }
          case Nil => Nil
        }

      children(enclosingTrees).head
    }
  }
  
  implicit class ExtendedSelection(selection: Selection){
    lazy val inboundDependencies = {
      val usedSymbols = selection.selectedSymbols
      val definedSymbols = selection.allSelectedTrees.collect{
        case t: DefTree => t.symbol
      }
      usedSymbols.diff(definedSymbols)
    }
  }

}