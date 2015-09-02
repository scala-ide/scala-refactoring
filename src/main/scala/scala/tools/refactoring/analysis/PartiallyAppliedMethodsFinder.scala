package scala.tools.refactoring
package analysis

/**
 * Provides methods to find DefDefs and ValDefs that are curried/partially applied
 * versions of other methods.
 */
trait PartiallyAppliedMethodsFinder {

  this: Indexes with common.CompilerAccess with common.PimpedTrees =>

  import global._

  case class DefInfo(symbol: Symbol, nrParamLists: Int, nrUntouchableParamLists: Int = 0)

  /**
   * Contains the common framework of PartialsFinder and PartialPartialsFinder
   * that fill in the missing parts.
   */
  trait GenericPartialsFinder[RHS] {

    /**
     * Finds the accessor method for the given ValDef.
     */
    def accessor(v: ValDef) = {
      v.symbol.getter(v.symbol.owner) match {
        case NoSymbol => None
        case sym => Some(sym)
      }
    }

    /**
     * Enables parameterization of pattern matchings.
     */
    trait Extractable[In, Out] {
      def unapply(i: In): Option[Out]
    }

    val ContainedSymbol: Extractable[RHS, Symbol]
    val Nested: Extractable[RHS, RHS]

    /**
     * Uses the extractors ContainedSymbol and Nested to decide
     * if a tree r of type RHS contains a call to the DefDef or
     * ValDef given by defSymbol.
     */
    def containsCall(r: RHS, defSymbol: Symbol): Boolean = r match {
      case ContainedSymbol(s) => defSymbol == s
      case Nested(n) => containsCall(n, defSymbol)
      case _ => false
    }

    /**
     * Returns the number of parameter lists that are left after
     * the currying call.
     */
    def nrParamLists(r: RHS, origNrParamLists: Int): Int

    val RHSExtractor: Extractable[Tree, RHS]

    val PartialDefDef = new Extractable[Tree, (DefDef, RHS)] {
      def unapply(tree: Tree) = Some(tree) collect {
        case defdef @ DefDef(_, _, _, _, _, RHSExtractor(rhs)) => (defdef, rhs)
      }
    }

    val PartialValDef = new Extractable[Tree, (ValDef, RHS)] {
      def unapply(tree: Tree) = Some(tree) collect {
        case valdef @ ValDef(_, _, _, RHSExtractor(rhs)) => (valdef, rhs)
      }
    }

    /**
     * Extracts partially applied calls from a candidate tree according to the
     * given extractors and uses the given handlers to process the matches.
     */
    def matchForPartial[T](
        candidate: Tree,
        defSymbol: Symbol,
        handleDefDefMatch: (DefDef, RHS) => T,
        handleValDefMatch: (ValDef, RHS) => T,
        handleNonMatch: => T): T = candidate match {
      case PartialDefDef(defdef, rhs) if containsCall(rhs, defSymbol) => handleDefDefMatch(defdef, rhs)
      case PartialValDef(valdef, rhs) if containsCall(rhs, defSymbol) => handleValDefMatch(valdef, rhs)
      case _ => handleNonMatch
    }

    def isPartialForSymbol(defSymbol: Symbol)(tree: Tree) =
      matchForPartial(tree, defSymbol, (d, r) => true, (v, r) => true, false)

    /**
     * Finds partials for a given def Symbol and its number of parameter lists.
     * Returns the def symbols of the partials found including
     * the number of parameter lists left.
     */
    def findPartialsForDef(partial: DefInfo) = {
      val defSymbol = partial.symbol
      val origNrParamLists = partial.nrParamLists
      val allOccurrences = index.occurences(defSymbol)
      val cuRoots = index.rootsOf(allOccurrences)
      val hits = cuRoots flatMap { cuRoot =>
        val traverser = new FilterTreeTraverser(isPartialForSymbol(defSymbol))
        traverser.traverse(cuRoot)
        traverser.hits.toList
      }

      val handleDefDef = (d: DefDef, f: RHS) => {
        val nrPLists = nrParamLists(f, origNrParamLists)
        val nrUntouchables = d.vparamss.size
        Some(DefInfo(d.symbol, nrPLists, nrUntouchables))
      }

      val handleValDef = (v: ValDef, f: RHS) => {
        val nrPLists = nrParamLists(f, origNrParamLists)
        accessor(v) map (DefInfo(_, nrPLists))
      }

      hits flatMap {
        matchForPartial(_, defSymbol, handleDefDef, handleValDef, None)
      }
    }

    /**
     * Indicates whether we need to search for partials recursively,
     * i.e. search for partials of partials...
     */
    val needsRecursion = false

    /**
     * Finds all partials for all given defs. Each provided def as well
     * as each found partial is represented by its symbol
     * and its number of parameter lists.
     */
    def findPartials(defs: List[DefInfo], acc: List[DefInfo] = Nil): List[DefInfo] = {
      val partials = defs flatMap findPartialsForDef
      if(needsRecursion) {
        partials match {
          case Nil => acc
          case ps => findPartials(partials, acc:::partials)
        }
      } else {
        partials
      }
    }

  }

  /**
   * Finds all DefDefs and ValDefs that are partial applications of a method.
   */
  object PartialsFinder extends GenericPartialsFinder[Function] {

    override val ContainedSymbol = new Extractable[Function, Symbol] {
      def unapply(f: Function) = Some(f) collect {
        case Function(_, a: Apply) => a.symbol
      }
    }

    override val Nested = new Extractable[Function, Function] {
      def unapply(f: Function): Option[Function] = Some(f) collect {
        case Function(_, g: Function) => g
      }
    }

    override def nrParamLists(f: Function, origNrParamLists: Int): Int = f match {
      case Function(_, g: Function) => 1 + nrParamLists(g, origNrParamLists)
      case _ => 1
    }

    override val RHSExtractor = new Extractable[Tree, Function] {
      def unapply(tree: Tree) = Some(tree) collect {
        case Block(_, fun: Function) => fun
        case Block(_, Block(_, fun: Function)) => fun
      }
    }

  }

  /**
   * Finds all DefDefs and ValDefs that are partial applications of a partially applied method,
   * i.e. all DefDefs and ValDefs that are partial applications of methods found by PartialsFinder.
   */
  object PartialPartialsFinder extends GenericPartialsFinder[Apply] {

    override val ContainedSymbol = new Extractable[Apply, Symbol] {
      def unapply(a: Apply) = Some(a) collect {
        case Apply(Select(s: Select, _), _) => s.symbol
      }
    }

    override val Nested = new Extractable[Apply, Apply] {
      def unapply(apply: Apply) = Some(apply) collect {
        case Apply(Select(a: Apply, _), _) => a
      }
    }

    override def nrParamLists(apply: Apply, origNrParamLists: Int): Int =
      origNrParamLists - nrAppliesInPartial(apply)

    def nrAppliesInPartial(apply: Apply): Int = apply match {
      case Apply(Select(qualifier: Apply, _), _) => 1 + nrAppliesInPartial(qualifier)
      case Apply(select: Select, _) => 1
      case _ => 0
    }

    override val RHSExtractor = new Extractable[Tree, Apply] {
      def unapply(tree: Tree) = Some(tree) collect {
        case a: Apply => a
        case Block(_, a: Apply) => a
      }
    }

    override val needsRecursion = true
  }
}