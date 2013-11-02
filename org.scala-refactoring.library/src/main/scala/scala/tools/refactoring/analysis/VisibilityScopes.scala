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
   *     p * x * y // <-- reference position
   *   }
   *   def c = 4
   * }
   * ```
   * 
   * ```
   * reference position -sees->
   *   Block(value y) -sees->
   *     Block(value x) -sees->
   *       DefDef(value p) -sees->
   *        Template(constructor A, value a, value b, value c) -sees->
   *          PackageDef(class A)
   * ```
   */
  case class VisibilityScope(
    /** The tree that encloses the visibility scope */
    enclosing: Tree, 
    /** All declarations in the visibility scope */
    declarations: List[DefTree], 
    /** The visibility scopes visible from this scope */
    visibleScopes: List[VisibilityScope]) {

    /** All symbols declared in this visibility scope */
    lazy val symbols =
      for (decl <- declarations) yield decl.symbol

    /** All symbols that are visible from this visibility scope */
    lazy val allVisibleSymbols: List[Symbol] =
      symbols ::: visibleScopes.flatMap(_.allVisibleSymbols)

    /** Removes visibility scopes that contains neither symbol declarations
     *  nor more than one child tree from the visibility tree.
     *  This method is only used for tree construction.
     */
    def condense: VisibilityScope = this match {
      case VisibilityScope(_, Nil, child :: Nil) =>
        child.condense
      case VisibilityScope(_, _, EmptyVisibilityTree :: Nil) =>
        this copy (visibleScopes = Nil)
      case _ =>
        this copy (visibleScopes = visibleScopes.map(_.condense))
    }

    override def toString = {
      enclosing.getClass().getSimpleName() + symbols.mkString("(", ", ", ")") + visibleScopes.mkString("{ ", ", ", " }")
    }
  }

  private val EmptyVisibilityTree = VisibilityScope(EmptyTree, Nil, Nil)

  def mkVisibilityScope(root: Tree, refPos: Position): VisibilityScope = {
    val enclosingTrees = {
      val traverser = new FilterTreeTraverser(cond(_) {
        case t => t.pos.includes(refPos)
      })
      traverser.traverse(root)
      traverser.hits.toList.reverse
    }

    def childrenFromBlock(block: Block, childTrees: List[VisibilityScope]): VisibilityScope = {
      def inner(stats: List[Tree]): List[VisibilityScope] = stats match {
        case (t: DefTree) :: rest if t.pos.point <= refPos.point =>
          VisibilityScope(block, t :: Nil, inner(rest)) :: Nil
        case _ :: rest => inner(rest)
        case Nil => childTrees
      }

      inner((block.stats ::: block.expr :: Nil).reverse).headOption.getOrElse(EmptyVisibilityTree)
    }

    val exclusionFlags =
      Flags.ACCESSOR

    def declarations(enclosing: Tree): List[DefTree] =
      enclosing match {
        case t: Tree =>
          t.children.collect {
            case dt: DefTree if !dt.symbol.hasFlag(exclusionFlags) && dt.symbol != NoSymbol => dt
          }
        case _ => Nil
      }

    def children(enclosingTrees: List[Tree]): List[VisibilityScope] =
      enclosingTrees match {
        case (b: Block) :: rest =>
          childrenFromBlock(b, children(rest)) :: Nil
        case t :: rest =>
          VisibilityScope(t, declarations(t), children(rest)) :: Nil
        case Nil =>
          EmptyVisibilityTree :: Nil
      }
    
    children(enclosingTrees).head.condense
  }

  /**
   * Enriches selections with easy access to visibility scopes.
   */
  implicit class SelectionWithVisibility(selection: Selection) {
    def visibilityScope() = {
      mkVisibilityScope(selection.root, selection.pos)
    }
  }

}