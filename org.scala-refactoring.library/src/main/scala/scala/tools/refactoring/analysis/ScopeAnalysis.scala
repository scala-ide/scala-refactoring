package scala.tools.refactoring.analysis

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.Selections

trait ScopeAnalysis extends Selections with CompilerAccess {
  import global._

  /**
   *
   */
  sealed trait ScopeTree {
    /**
     * A tree that fully encloses this scope.
     */
    val enclosing: Tree

    /**
     * A scope that contains this scope in the "physical" structure.
     */
    val outerScope: Option[ScopeTree]

    /**
     * Symbols from which we know that they are visible in this scope.
     */
    val knownSymbols: List[Symbol]

    /**
     * Scopes that are visible from this scope.
     */
    val visibleScopes: List[ScopeTree] =
      outerScope.toList ::: outerScope.map(_.visibleScopes).getOrElse(Nil)

    /**
     * The outermost scope known. Usually the package scope of the CU.
     */
    lazy val outermostScope: ScopeTree =
      outerScope.map(_.outermostScope).getOrElse(this)

    /**
     * Is `s` visible (and accessible) from this scope?
     */
    def sees(s: Symbol): Boolean = {
      knownSymbols.contains(s) ||
        introducedInThisScope(s) ||
        visibleScopes.exists(_.sees(s))
    }

    /**
     * Is `s` visible in this scope, but not in `outerScope`?
     */
    def introducedInThisScope(s: Symbol): Boolean

    /**
     * Returns a list of visible symbols whose name is equal
     * to `name`.
     * TODO: Handle symbols from other CUs
     */
    def nameCollisions(name: String): List[Symbol] = {
      (enclosing.children.flatMap {
        case t: Template => t.children.filter(_.hasSymbol)
        case t: DefTree => t :: Nil
        case _ => Nil
      }.map(_.symbol).filter(_.decodedName == name)) :::
        knownSymbols.filter(_.decodedName == name) :::
        outerScope.map(_.nameCollisions(name)).getOrElse(Nil)
    }

    /**
     * Returns a name based on `preferred` that does not collide
     * with other visible symbols.
     */
    def proposeName(preferred: String): String =
      if (nameCollisions(preferred).isEmpty)
        preferred
      else
        (1 to Int.MaxValue).collectFirst {
          case i if nameCollisions(preferred + i).isEmpty =>
            preferred + i
        }.get

    /**
     * Traverses the scope tree from inner to outer and returns
     * the innermost scope, that is visible from `t`.
     */
    def findScopeFor(t: Tree): ScopeTree =
      findScopeFor(t.pos)

    /**
     * Traverses the scope tree from inner to outer and returns
     * the innermost scope, that is visible from `pos`.
     */
    def findScopeFor(pos: Position): ScopeTree =
      outerScope.map(_.findScopeFor(pos)).getOrElse(this)

    /**
     * Returns a copy of this scope with `syms` registered as known
     * symbols. Also propagates all symbols of `syms`, that are not introduces
     * in this scope to `outerScope`.
     *
     * Register symbols as known symbols increases the performance of `sees(s)`
     * and helps to reduce the number of false negatives. This is because it is
     * easier to determine if a symbol is introduced by a scope than if it is
     * not introduced by it.
     */
    def withKnownSymbols(syms: List[Symbol]): ScopeTree = {
      val outerWithSyms = outerScope.map(_.withKnownSymbols(syms.filterNot(introducedInThisScope(_))))
      this.copy(outerScope = outerWithSyms, knownSymbols = syms)
    }

    def copy(outerScope: Option[ScopeTree] = outerScope, knownSymbols: List[Symbol] = knownSymbols): ScopeTree

    def toString(className: String, identName: String): String =
      s"$className($identName)" + (outerScope match {
        case None => ""
        case Some(s) => " -> " + s.toString()
      })
  }

  /**
   * Traits, classes, objects and packages are transformed to MemberScopes.
   *
   * MemberScopes are not structure sensitive. Therefore a member is also
   * visible at positions before its definition.
   */
  case class MemberScope(enclosing: Tree, outerScope: Option[ScopeTree], knownSymbols: List[Symbol] = Nil) extends ScopeTree {
    assert(enclosing.isInstanceOf[ImplDef] || enclosing.isInstanceOf[PackageDef])

    def introducedInThisScope(s: Symbol) = {
      s.owner.ownerChain.toString == enclosing.symbol.ownerChain.toString
    }

    override def findScopeFor(pos: Position) =
      if (enclosing.pos.includes(pos))
        this
      else
        super.findScopeFor(pos)

    def copy(outerScope: Option[ScopeTree], knownSymbols: List[Symbol]) =
      MemberScope(enclosing, outerScope, knownSymbols)

    override def toString =
      toString("MemberScope", enclosing.symbol.decodedName)
  }

  /**
   * Declarations of values and types that are not class members are
   * transformed to LocalScopes. This also includes parameter lists
   * and bindings in `case` statements.
   *
   * One LocalScope can also represent more than one declaration if it
   * is constructed from parameter lists or bindings.
   */
  case class LocalScope(enclosing: Tree, decls: List[DefTree], outerScope: Option[ScopeTree], knownSymbols: List[Symbol] = Nil) extends ScopeTree {

    def introducedInThisScope(s: Symbol) =
      decls.exists(_.symbol == s)

    override def findScopeFor(pos: Position) =
      if (enclosing.pos.includes(pos) && pos.startOrPoint > declPos.startOrPoint)
        this
      else
        super.findScopeFor(pos)

    val declPos = decls match {
      case Nil => enclosing.pos
      case d :: _ => d.pos
    }

    def copy(outerScope: Option[ScopeTree], knownSymbols: List[Symbol]) =
      LocalScope(enclosing, decls, outerScope, knownSymbols)

    override def toString =
      toString("LocalScope", decls.map(_.symbol.decodedName).mkString(", "))
  }

  object ScopeTree {
    /**
     * Constructs a scope tree from `root` towards `ref`.
     */
    def build(root: Tree, ref: Tree): ScopeTree = build(ref, root :: Nil, None, root)

    /**
     * Constructs a scope tree for selection `s` and
     * registers all inbound dependencies as known symbols.
     */
    def build(s: Selection): ScopeTree =
      build(s.root, s.selectedTopLevelTrees.head)
        .withKnownSymbols(s.inboundDeps)

    private def build(ref: Tree, ts: List[Tree], outerScope: Option[ScopeTree], enclosing: Tree): ScopeTree =
      ts match {
        case Nil => outerScope.get
        case t :: rest if t.pos.includes(ref.pos) =>
          t match {
            case _: ImplDef | _: PackageDef =>
              build(ref, t.children, Some(MemberScope(t, outerScope)), t)

            case d: DefDef =>
              // local methods form two scopes. The outer one is the local method
              // declaration and the inner one is the parameter list.
              // This sounds like it is possible to recursively call a function
              // from its parameter list. And...yeah it is:
              // 	def fn(a: Int = fn(7)) = a - 1
              val outer =
                if (d.symbol.isLocal)
                  Some(LocalScope(enclosing, d :: Nil, outerScope))
                else
                  outerScope
              val pScope =
                if (d.vparamss.isEmpty)
                  outerScope
                else
                  Some(LocalScope(d, d.vparamss.flatten, outer))
              build(ref, d.children, pScope, d)

            case d: Function =>
              val pScope =
                if (d.vparams.isEmpty)
                  outerScope
                else
                  Some(LocalScope(d, d.vparams, outerScope))
              build(ref, d.children, pScope, d)

            case c @ CaseDef(pat, _, _) =>
              val bindings = pat.collect {
                case t: DefTree => t
              }
              build(ref, c.children, Some(LocalScope(c, bindings, outerScope)), c)

            case d: DefTree if d.symbol.isLocal && !d.symbol.isValueParameter =>
              build(ref, d.children, Some(LocalScope(enclosing, d :: Nil, outerScope)), d)

            case _ => build(ref, t.children, outerScope, t)
          }
        case t :: rest if t.pos.isRange && t.pos.start < ref.pos.start =>
          t match {
            case d: DefTree if d.symbol.isLocal && !d.symbol.isValueParameter =>
              build(ref, rest, Some(LocalScope(enclosing, d :: Nil, outerScope)), enclosing)

            case _ =>
              build(ref, rest, outerScope, enclosing)
          }
        case _ :: rest => build(ref, rest, outerScope, enclosing)
      }
  }
}