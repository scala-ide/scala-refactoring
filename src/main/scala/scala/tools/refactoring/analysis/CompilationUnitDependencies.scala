/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import scala.tools.refactoring.common.CompilerApiExtensions
import scala.tools.refactoring.common.TracingImpl
import scala.annotation.tailrec

trait CompilationUnitDependencies extends CompilerApiExtensions with ScalaVersionAdapters.CompilerApiAdapters with TracingImpl {
  // we need to interactive compiler because we work with RangePositions
  this: common.InteractiveScalaCompiler with common.TreeTraverser with common.TreeExtractors with common.PimpedTrees =>

  import global._

  def isQualifierDefaultImported(t: Tree) = {
    t match {
      case t: Select =>
        val Scala = newTypeName("scala")
        t.qualifier match {
          case Select(This(Scala), _) => true
          case qual if qual.nameString == "ClassTag" => true
          case q => q.symbol.isOmittablePrefix
        }
      case _ => false
    }
  } \\ { res =>
    if (res) {
      trace(s"Qualifier $t is default imported")
    }
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
  } \\ { res =>
    if (!res) {
      trace(s"Import $t not really needed")
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

    val deps = dependencies(t)

    val neededDependencies = deps.flatMap {
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
    val wholeTree = t

    def qualifierIsEnclosingPackage(t: Select) = {
      enclosingPackage(wholeTree, t.pos) match {
        case pkgDef: PackageDef =>
          t.qualifier.nameString == pkgDef.nameString
        case _ => false
      }
    }

    def isDefinedLocally(t: Tree): Boolean = isSymDefinedLocally(t.symbol)

    def isSymDefinedLocally(sym: Symbol): Boolean = wholeTree.exists {
      case defTree: DefTree if sym == defTree.symbol => true
      case _ => false
    }

    def isDefinedLocallyAndQualifiedWithEnclosingPackage(t: Select) = {
      qualifierIsEnclosingPackage(t) && isDefinedLocally(t)
    }

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
      var annotationTree: Option[Tree] = None

      def traversingAnnotation() = annotationTree.isDefined

      def tryFindTpeAndSymFor(ident: Ident) = {
        Option(ident.tpe).map((_, ident.symbol)).orElse {
          annotationTree.flatMap { tree =>
            val name = ident.name

            tree.collect {
              case Literal(Constant(tpe @ TypeRef(_, sym, _))) if (sym.name == name) => (tpe, sym)
            }.headOption
          }
        }
      }

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
          case t @ Select(qual, _) if !isRelativeToLocalImports(qual) =>
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

      def isScopeForLocalImports(tree: Tree) = tree match {
        case _: ImplDef | _: ValOrDefDef => true
        case _ => false
      }

      var inScopeForLocalImports = false
      var localImports: List[Import] = Nil
      var localImportsForParent: List[Import] = Nil
      var topLevelImports: List[Import] = Nil
      var pkgDefStack: List[RefTree] = Nil

      def isRelativeToLocalImports(tree: Tree): Boolean = {
        (hasImplicitQualifier(tree) && isRelativeTo(localImports, tree)) \\ { res =>
          trace("Tree %s is relative to local imports: %s", tree, res)
        }
      }

      def isRelativeToTopLevelImports(tree: Tree): Boolean = {
        (hasImplicitQualifier(tree) && !isRelativeTo(localImports, tree) && isRelativeTo(topLevelImports, tree)) \\ { res =>
          trace("Tree %s is relative to top level imports: %s", tree, res)
        }
      }

      def isRelativeToEnclosingPackage(tree: Tree): Boolean = {
        !hasImplicitQualifier(tree) && {
          val owningPkg = getOwningPkg(tree.symbol)

          pkgDefStack.exists { pid =>
            owningPkg.toString == pid.symbol.toString
          }
        }
      } \\ { res =>
        trace("Tree %s is relative to enclosing package: %s", tree, res)
      }

      @tailrec
      def getOwningPkg(symbol: Symbol): Symbol = {
        if (symbol.hasPackageFlag) symbol
        else getOwningPkg(symbol.owner)
      }

      def isRelativeTo(imports: List[Import], tree: Tree): Boolean = {
        imports.exists { imp =>
          def compareSyms = imp.expr.symbol == tree.symbol

          imp.selectors match {
            case List(singleSelector) =>
              tree match {
                case Select(q, n) =>
                  q.symbol == imp.expr.symbol && (n == singleSelector.name || singleSelector.name == nme.WILDCARD)
                case _ => compareSyms
              }
            case _ => compareSyms
          }
        }
      }

      @tailrec
      def hasImplicitQualifier(tree: Tree): Boolean = tree match {
        case Select(q, _) =>
          if (q.pos.isRange) hasImplicitQualifier(q)
          else true
        case _: Ident => !tree.pos.isRange
        case _ => false
      }

      override def traverse(tree: Tree) = {
        var resetInScopeForLocalImports = false
        var restoreLocalImports = false
        var popPkgDefStack = false

        if (isScopeForLocalImports(tree)) {
          localImportsForParent = localImports
          restoreLocalImports = true

          if (!inScopeForLocalImports) {
            inScopeForLocalImports = true
            resetInScopeForLocalImports = true
          }
        }

        try {
          tree match {
            // Always add the SIP 18 language imports as required until we can handle them properly
            case Import(select @ Select(Ident(nme.scala_), `language`), feature) =>
              feature foreach (selector => addToResult(Select(select, selector.name)))

            case imp @ Import(qualifier, selector)  => {
              if (inScopeForLocalImports) {
                if (!qualifier.symbol.isLocal && !qualifier.symbol.isVal) {
                  if (isRelativeToTopLevelImports(qualifier) || isRelativeToEnclosingPackage(qualifier)) {
                    fakeSelectTree(qualifier.tpe, qualifier.symbol, qualifier) match {
                      case select: Select => addToResult(select)
                      case _ => ()
                    }
                  }
                }
                localImports ::= imp
              } else {
                topLevelImports ::= imp
              }
            }

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
            case t @ Select(qual: Select, nme.apply) if (qual.pos.isTransparent && t.pos.isOpaqueRange) || qual.pos.isOpaqueRange =>
              if(hasStableQualifier(qual) && !isSelectFromInvisibleThis(qual.qualifier)) {
                addToResult(qual)
              }

            case t @ Select(qual, _) if t.pos.isOpaqueRange =>
              if (!isMethodCallFromExplicitReceiver(t)
                  && !isSelectFromInvisibleThis(qual)
                  && t.name != nme.WILDCARD
                  && hasStableQualifier(t)
                  && !t.symbol.isLocal
                  && !isRelativeToLocalImports(t)
                  && !isDefinedLocallyAndQualifiedWithEnclosingPackage(t)) {
                addToResult(t)
              }

              super.traverse(t)

            // workaround for Assembla ticket #1002402
            case t @ ValDef(modifiers, _, tpe: TypeTree, rhs) if modifiers.isLazy =>
              tpe.original match {
                case s @ Select(qualifier, _) =>
                  addToResult(s)
                  super.traverse(rhs)
                case _ => super.traverse(t)
              }

            /*
             * classOf[some.Type] is represented by a Literal
             * */
            case t @ Literal(c @ Constant(value)) =>
              value match {
                case tpe @ TypeRef(_, sym, _) =>
                  fakeSelectTreeFromType(tpe, sym, t.pos) match {
                    case s: Select if !isDefinedLocallyAndQualifiedWithEnclosingPackage(s) =>
                      addToResult(s)
                    case _ => ()
                  }
                case _ => ()
              }

            case t: Ident if t.name != nme.EMPTY_PACKAGE_NAME && t.name != tpnme.WILDCARD_STAR  =>
              tryFindTpeAndSymFor(t) match {
                case Some((tpe, sym)) =>
                  fakeSelectTree(tpe, sym, t) match {
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

            case t: Ident => {
              super.traverse(tree)
            }

            case t @ PackageDef(pid, stats) =>
              pkgDefStack ::= pid
              popPkgDefStack = true
              stats.foreach(traverse)

            case _ =>
              super.traverse(tree)
          }
        } finally {
          if (resetInScopeForLocalImports) {
            inScopeForLocalImports = false
          }

          if (restoreLocalImports) {
            localImports = localImportsForParent
          }

          if (popPkgDefStack) {
            pkgDefStack = pkgDefStack.tail
          }
        }
      }

      override def handleAnnotations(as: List[AnnotationInfo]): Unit = {
        val recusing = annotationTree.isDefined

        if (!recusing) {
          try {
            as.foreach { a =>
              val tree = annotationInfoTree(a)
              val orig = a.original

              annotationTree = Some(tree)
              traverse(orig)
            }
          } finally {
            annotationTree = None
          }
        }
      }
    }

    traverser.traverse(t)

    val deps = result.values.toList

    deps.filterNot(_.symbol.isPackage).toList
  }
}

