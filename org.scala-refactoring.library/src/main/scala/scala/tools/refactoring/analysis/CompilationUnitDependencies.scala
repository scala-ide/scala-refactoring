/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

trait CompilationUnitDependencies {
  // we need to interactive compiler because we work with RangePositions
  this: common.InteractiveScalaCompiler with common.TreeTraverser with common.TreeExtractors with common.PimpedTrees =>

  import global._

  def isQualifierDefaultImported(t: Tree) = !isQualifierNotDefaultImported(t)

  def isQualifierNotDefaultImported(t: Tree) = t match {
    case t: Select =>
      val Scala = newTypeName("scala")
      val Java = newTypeName("java")
      val `lang` = newTermName("lang")
      t.qualifier match {
        case Ident(Names.scala) => false
        case This(Scala) => false
        case Select(This(Java), `lang`) => false
        case Select(Ident(Names.scala), Names.pkg) => false
        case Select(Ident(Names.scala), Names.Predef) => false
        case Select(This(Scala), _) => false
        case qual if qual.symbol.isSynthetic && !qual.symbol.isModule => false
        case qual if qual.nameString == "ClassTag" => false
        case _ => true
      }
    case _ => true
  }

  /**
   * Helper function to filter out trees that we don't need
   * to import, for example because they come from Predef.
   */
  def isImportReallyNeeded(t: Select) = {

    val lastSymbol = t.filter(_ => true).last.symbol

    if(lastSymbol != NoSymbol && lastSymbol.isLocal) {
      // Our import "chain" starts from a local value,
      // so we cannot import `t` globally.
      false
    } else {
      !isQualifierDefaultImported(t)
    }
  }

  /**
   * Finds the last "visible" (with a range position) select in some tree selection.
   *
   * Selects are usually only partly written down in source code (except when we write
   * down the full name to some identifier), so there exists a select at which the tree
   * turns from being visible to being invisible. We need to find this tree to determine
   * whether we need do make an import with the minimally required path.
   *
   * This function also already filters trees that we don't need to import, e.g. from the
   * Predef or the scala package.
   */
  def findDeepestNeededSelect(t: Tree): Option[Select] = t match {
    case selected @ Select(qual @ Select(underlying, _), _) if qual.symbol.isPackageObject && underlying.pos.isRange =>

      /* When importing from a package object, e.g. scala.sys.`package`.error, the `package` select
       * doesn't have a position. So we "skip" this package object and continue with the underlying
       * select, which might again reveal a range position.
       */
      findDeepestNeededSelect(underlying)

    case s @ Select(qual, name) if s.pos.isRange && !qual.pos.isOpaqueRange =>
      Some(s)
    case s: Select =>
      findDeepestNeededSelect(s.qualifier)
    case _ =>
      None
  }

  /**
   * Calculates a list of all needed imports for the given Tree.
   */
  def neededImports(t: Tree): List[Select] = {

    /**
     * Check if the definition is also a child of the outer `t`. In that case, we don't need
     * to add an import because the dependency is to a local definition.
     */
    def isLocalDefinition(dependency: Tree) = t.exists {
      case t: DefTree => dependency.symbol == t.symbol
      case _ => false
    }

    val deps = dependencies(t)

    val neededDependencies = deps.flatMap {
      case t if isLocalDefinition(t) => None
      case t: Select if !t.pos.isRange => Some(t)
      case t => findDeepestNeededSelect(t)
    }.filter(isImportReallyNeeded).distinct

    // Eliminate duplicates by converting them to strings.
    neededDependencies.groupBy(asSelectorString).map(_._2.head).toList filterNot (_.symbol == t.symbol)
  }

  /**
   * Calculates all the external dependencies the given Tree has.
   * Compared to `neededImports`, this function might also return
   * trees that don't need to be explicitly imported, for example
   * because they are defined in the same compilation unit.
   */
  def dependencies(t: Tree): List[Select] = {

    val result = new collection.mutable.HashMap[String, Select]

    def addToResult(t1: Select) = {
      val key = t1.toString
      result.get(key) match {
        case None =>
          result += (key -> t1)
        case Some(t2) =>
          val lengthOfVisibleQualifiers1 = t1.filter(_.pos.isRange)
          val lengthOfVisibleQualifiers2 = t2.filter(_.pos.isRange)

          if(lengthOfVisibleQualifiers1.size < lengthOfVisibleQualifiers2.size) {
            // If we have an imported type that is used with the full package
            // name but also with the imported name, we keep the one without the
            // package so we don't incorrectly remove the import.
            result += (key -> t1)
          }
      }
    }

    val traverser = new TraverserWithFakedTrees {

      def isSelectFromInvisibleThis(t: Tree) = {

        val isInTopLevelPackage = Set("java", "scala").contains _

        t.exists {
          case t: This if isInTopLevelPackage(t.qual.toString) =>
            false
          case t: This =>
            !t.pos.isOpaqueRange
          case _ => false
        }
      }

      def foundPotentialTree(t: Tree) = {
        t match {
          case Select(Ident(name), _) if name startsWith nme.EVIDENCE_PARAM_PREFIX =>
            ()
          case t: Select =>
            addToResult(t)
          case _ =>
            ()
        }
      }

      def handleSelectFromImplicit(t: Tree) = {
        val selects = t.find {
          case t: Select =>
            !isSelectFromInvisibleThis(t)
          case _ => false
        }
        selects foreach foundPotentialTree
      }

      // we don't need to add a dependency for method calls where the receiver
      // is explicit in the source code.
      def isMethodCallFromExplicitReceiver(t: Select) = t match {
        case Select(qual, nme.apply) =>
          t.qualifier.pos.isRange
        case t =>
          t.qualifier.pos.isRange && t.symbol.isMethod &&
          !(t.qualifier.pos.sameRange(t.pos) && t.qualifier.pos.isTransparent)
      }

      def hasStableQualifier(t: Select) = {
        t.qualifier.symbol != null && (!t.qualifier.symbol.isTerm || t.qualifier.symbol.isStable)
      }

      val language = newTermName("language")

      override def traverse(tree: Tree) = tree match {

        // Always add the SIP 18 language imports as required until we can handle them properly
        case Import(select @ Select(Ident(nme.scala_), `language`), feature) =>
          feature foreach (selector => addToResult(Select(select, selector.name)))

        case Import(_, _) => ()

        case Select(Ident(Names.scala), _) => ()

        case Select(Select(Ident(Names.scala), Names.pkg), _) => ()

        case t: Template =>
          // The primary constructor can have visible annotations even
          // if its DefDef has an OffsetPosition.
          t.primaryConstructor foreach {
            case primaryConstructor: DefDef =>
              handleAnnotations(primaryConstructor.symbol.annotations)
          }

          super.traverse(tree)

        case t : ApplyImplicitView =>

          val hasSelectFromNonPackageThis = t.fun exists {
            case Select(ths: This, _) if !ths.symbol.isPackageClass =>
              true
            case _ =>
              false
          }

          if(hasSelectFromNonPackageThis) {
            // We can ignore this tree because it is selected from a
            // non-package instance, so there's nothing useful to import.
          } else {

            t.fun find {
              case t: Select =>
                !isMethodCallFromExplicitReceiver(t) && !t.pos.isTransparent && hasStableQualifier(t)
              case _ =>
                false
            } foreach foundPotentialTree
          }

          t.args foreach traverse

        case t : ApplyToImplicitArgs =>
          traverse(t.fun)
          t.args foreach handleSelectFromImplicit

        case Select(New(qual), _) =>
          traverse(qual)

        // workaround for SI-5064
        case t @ Select(qual: This, _) if qual.pos.sameRange(t.pos) =>
          ()

        // workaround for SI-5064
        case t @ Select(qual: Select, nme.apply) if qual.pos.isTransparent && t.pos.isOpaqueRange =>
          if(hasStableQualifier(qual) && !isSelectFromInvisibleThis(qual.qualifier)) {
            addToResult(qual)
          }

        case t @ Select(qual, _) if t.pos.isOpaqueRange =>

          if (!isMethodCallFromExplicitReceiver(t)
              && !isSelectFromInvisibleThis(qual)
              && t.name != nme.WILDCARD
              && hasStableQualifier(t)) {
            addToResult(t)
          }

          super.traverse(t)

        /*
         * classOf[some.Type] is represented by a Literal
         * */
        case t @ Literal(Constant(value)) =>

          value match {
            case tpe @ TypeRef(_, sym, _) =>
              fakeSelectTreeFromType(tpe, sym, t.pos) match {
                case t: Select => addToResult(t)
                case _ => ()
              }
            case _ => ()
          }

        case t: Ident if t.tpe != null && t.name != nme.EMPTY_PACKAGE_NAME =>
          fakeSelectTree(t.tpe, t.symbol, t) match {
            // only repeat if it's a Select, if it's an Ident,
            // otherwise we risk to loop endlessly
            case select: Select =>
              traverse(select)
            case _ =>
              super.traverse(tree)
          }

        case _ =>
          super.traverse(tree)
      }
    }

    traverser.traverse(t)

    val deps = result.values.toList

    deps.filterNot(_.symbol.isPackage).toList
  }
}

