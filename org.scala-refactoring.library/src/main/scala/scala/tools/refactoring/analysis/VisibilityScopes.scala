package scala.tools.refactoring.analysis

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.Selections
import scala.reflect.internal.Flags
import PartialFunction._

trait VisibilityScopes extends Selections { self: CompilerAccess =>
  import global._

  /**
   * A data structure that represents the visible symbols at a given position.
   * Currently imported symbols and symbols from super classes or traits are
   * not tracked by the visibility scopes.
   *
   * Visibility scopes keep also track of inbound dependencies of the reference
   * selection that was used to build the visibility tree. See `definedDependencies`
   * and `undefinedDependencies`.
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
   *   BlockScope(value y) -sees->
   *     BlockScope(value x) -sees->
   *       MethodScope(value p) -sees->
   *        TemplateScope(constructor A, value a, value b, value c) -sees->
   *          PackageScope(class A)
   * ```
   */
  sealed trait VisibilityScope extends Traversable[VisibilityScope] {
    /**
     * The selection that was used to build this scope
     */
    val referenceSelection: Selection

    /**
     * The tree that encloses this visibility scope
     */
    val enclosing: Tree

    val allEnclosingTrees: List[Tree]

    /**
     * Inbound dependencies of `referenceSelection` that are visible from
     * this scope.
     */
    val definedDependencies: List[Symbol]

    /**
     * Inbound dependencies that are not visible from this scope.
     */
    val undefinedDependencies: List[Symbol] =
      referenceSelection.inboundDeps diff definedDependencies

    /**
     * The visibility scopes visible from this scope
     */
    lazy val visibleScopes: List[VisibilityScope] = {
      val definedDepsWithoutSymbols =
        definedDependencies filterNot {
          case s if s.isAccessor => symbols.contains(s.accessed) || symbols.contains(s)
          case s => symbols.contains(s)
        }

      VisibilityScope(referenceSelection, allEnclosingTrees.tail, definedDepsWithoutSymbols)
    }

    /**
     * All declarations in this visibility scope
     */
    private val declarations: List[DefTree] = this match {
      case BlockScope(_, enclosing, _, _) =>
        val stats = enclosing.stats :+ enclosing.expr
        stats.collect {
          case t: DefTree if t.pos.start < referenceSelection.pos.start => t
        }
      case CaseScope(_, enclosing, _, _) =>
        enclosing.pat.collect {
          case b: Bind => b
        }
      case _ =>
        enclosing.children.collect {
          case dt: DefTree if !dt.symbol.hasFlag(Flags.ACCESSOR) && dt.symbol != NoSymbol =>
            dt
        }
    }

    /**
     * All symbols declared in this visibility scope
     */
    lazy val symbols =
      for (decl <- declarations) yield decl.symbol

    /**
     * All symbols that are visible from this visibility scope
     */
    lazy val allVisibleSymbols: List[Symbol] =
      symbols ::: visibleScopes.flatMap(_.allVisibleSymbols)

    /**
     * Gets all symbols with the name `name`
     */
    def termNameCollisions(name: String): List[Symbol] = this match {
      case _ => allVisibleSymbols.filter { vsym =>
        vsym.isTerm && vsym.name.decode.trim == name
      }
    }

    /**
     * A meaningful, short description of this scope
     */
    lazy val name = this match {
      case _: PackageScope => s"Package ${enclosing.nameString}"
      case TemplateScope(_, c: ClassDef, _, _, _) => s"Class ${c.symbol.name.decode}"
      case TemplateScope(_, c: ModuleDef, _, _, _) => s"Object ${c.symbol.name.decode}"
      case _: MethodScope => s"Method ${enclosing.nameString}"
      case _ => "Local Scope"
    }

    /**
     * Describes what kind of scope it is and which symbols are defined in this scope
     */
    override def toString = {
      getClass().getSimpleName() + symbols.mkString("(", ", ", ")") + visibleScopes.mkString("{ ", ", ", " }")
    }

    /**
     * Enables depth first traversing of visibility scopes.
     */
    def foreach[U](f: VisibilityScope => U): Unit = {
      f(this)
      for (child <- visibleScopes) {
        child.foreach(f)
      }
    }
  }

  case class PackageScope(
    val referenceSelection: Selection,
    val enclosing: PackageDef,
    val allEnclosingTrees: List[Tree],
    val definedDependencies: List[Symbol]) extends VisibilityScope

  case class TemplateScope(
    val referenceSelection: Selection,
    val classOrModule: ImplDef,
    val enclosing: Template,
    val allEnclosingTrees: List[Tree],
    val definedDependencies: List[Symbol]) extends VisibilityScope

  case class MethodScope(
    val referenceSelection: Selection,
    val enclosing: DefDef,
    val allEnclosingTrees: List[Tree],
    val definedDependencies: List[Symbol]) extends VisibilityScope

  case class FunctionScope(
    val referenceSelection: Selection,
    val enclosing: Function,
    val allEnclosingTrees: List[Tree],
    val definedDependencies: List[Symbol]) extends VisibilityScope

  case class BlockScope(
    val referenceSelection: Selection,
    val enclosing: Block,
    val allEnclosingTrees: List[Tree],
    val definedDependencies: List[Symbol]) extends VisibilityScope

  case class CaseScope(
    val referenceSelection: Selection,
    val enclosing: CaseDef,
    val allEnclosingTrees: List[Tree],
    val definedDependencies: List[Symbol]) extends VisibilityScope

  /**
   * VisibilityScope constructors
   */
  object VisibilityScope {
    def apply(s: Selection): VisibilityScope = {
      val enclosingTrees = {
        val traverser = new FilterTreeTraverser(cond(_) {
          case t => t.pos.includes(s.pos) && !t.pos.sameRange(s.pos)
        })
        traverser.traverse(s.root)
        traverser.hits.toList.reverse
      }

      apply(s, enclosingTrees, s.inboundDeps).head
    }

    def apply(s: Selection, enclosingTrees: List[Tree], definedDependencies: List[Symbol]): List[VisibilityScope] =
      enclosingTrees match {
        case t :: rest =>
          t match {
            case t: PackageDef => PackageScope(s, t, enclosingTrees, definedDependencies) :: Nil
            case impl: ImplDef => TemplateScope(s, impl, impl.impl, enclosingTrees, definedDependencies) :: Nil
            case t: DefDef => MethodScope(s, t, enclosingTrees, definedDependencies) :: Nil
            case t: Function => FunctionScope(s, t, enclosingTrees, definedDependencies) :: Nil
            case t: Block => BlockScope(s, t, enclosingTrees, definedDependencies) :: Nil
            case t @ CaseDef(pat, _, _) => CaseScope(s, t, enclosingTrees, definedDependencies) :: Nil
            case _ => apply(s, rest, definedDependencies)
          }
        case Nil => Nil
      }
  }

}