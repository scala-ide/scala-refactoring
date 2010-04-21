/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.symtab.Symbols
import scala.collection.mutable.{HashMap, ListBuffer}

trait FullIndexes extends Indexes {

  val global: scala.tools.nsc.Global
  import global._

  val index = new Index {

    private type Refs = ListBuffer[Tree]
    private type Defs = ListBuffer[DefTree]

    private val defs = new HashMap[Symbol, DefTree]
    private val refs = new HashMap[Symbol, Refs]
    private val chld = new HashMap[Symbol, Defs]

    private def dbgSymbol(sym: Symbol) = "Symbol " + sym.nameString + " [" + sym.id + "] (" + sym.pos + ") {" + sym.info + "}"
    private def dbgTree(t: Tree) = t.getClass.getSimpleName + " " + t.toString.replaceAll("\n", " ") + " (" + t.pos + ")"

    def debugString() = {

      val ds = defs map {
        case (sym, t) =>
          dbgSymbol(sym) + " is defined by: \n -" + dbgTree(t)
      } mkString "\n\n"

      val rs = refs map {
        case (sym, refs) =>
          dbgSymbol(sym) + " is referenced by: \n -" + (refs map dbgTree mkString "\n -")
      } mkString "\n\n"

      ds + "\n\n====\n\n" + rs
    }

    def declaration(s: Symbol): Option[DefTree] = defs.get(s)

    def references(s: Symbol) = occurences(s) filterNot (Some(_) == declaration(s))

    def occurences(s: Symbol) = {
      s :: dependentSymbols(s) flatMap { s =>
        declaration(s).toList ::: refs.getOrElse(s, new Refs).toList ::: findInHierarchy(s)
      } filter (_.pos.isRange) distinct
    }

    private def dependentSymbols(s: Symbol) = {

      def superClassParameters(s: Symbol): List[Symbol] = s match {
        case _ if s != NoSymbol && s.owner.isClass && s.isGetterOrSetter =>

          List(declaration(s.owner)) collect {
            case Some(ClassDef(_, _, _, Template(_, _, body))) => body collect {
              case d@DefDef(_, _, _, _, _, Block(stats, _)) if d.symbol.isConstructor => stats collect {
                case Apply(_, args) => args collect {
                  case symTree: SymTree if symTree.symbol.nameString == s.nameString => symTree.symbol
                }
              } flatten
            } flatten
          } flatten

        case _ => Nil
      }

      def resolveGetterImplRelations(s: Symbol) = s match {
        case _ if s.isGetterOrSetter =>
          s.accessed :: Nil
        case _ if s.owner != NoSymbol =>
          s.getter(s.owner) :: s.setter(s.owner) :: Nil
        case _ =>
          Nil
      }

      def samePosition(ss: Iterable[Symbol], s: Symbol): List[Symbol] = {
        ss collect {
          case sym if sym.pos.sameRange(s.pos) && sym.pos.source.file.name == s.pos.source.file.name && !sym.pos.isTransparent =>
            sym
        } toList
      }

      def companion(s: Symbol) = s.companionSymbol

      companion(s) ::
        superClassParameters(s) :::
        resolveGetterImplRelations(s) :::
        samePosition(defs.keys, s) :::
        samePosition(refs.keys, s) filter (_ != NoSymbol)
    }

    private def findInHierarchy(s: Symbol) = s match {
      case s: TermSymbol if s.owner.isClass =>
      
        def allSubClasses(clazz: Symbol) = defs.filter {
          case (defn, _) if defn.isClass => 
            // it seems like I can't compare these symbols with ==?
            defn.ancestors.exists(t => t.pos.sameRange(clazz.pos) && t.pos.source == clazz.pos.source)
          case _ => false
        }
        
        val alls = allSubClasses(s.owner)
                
        val overrides = allSubClasses(s.owner) map {
          case (otherClass, _) =>
            try {
              s overriddenSymbol otherClass
            } catch {
              case e: Error =>
                // ?
                throw e
            }
        } filter (_ != NoSymbol) toList

        overrides flatMap occurences distinct
      case _ => Nil
    }

    def children(s: Symbol) = chld.getOrElse(s, new Defs) toList

    def processTree(t: Tree): Unit = {

      def addDefinition(t: DefTree) = {
        defs += t.symbol → t
        chld.getOrElseUpdate(t.symbol.owner, new Defs) += t
      }

      def addReference(s: Symbol, t: Tree): Unit = refs.getOrElseUpdate(s, new Refs) += t

      t foreach {
        // The standard traverser does not traverse a TypeTree's original:
        case t: TypeTree if t.original != null =>
          processTree(t.original)

          // Special treatment for type ascription
          (t.original, t.tpe) match {
            case (AppliedTypeTree(_, args1), TypeRef(_, _, args2)) =>
              args1 zip args2 collect {
                case (i: RefTree, tpe: TypeRef) => addReference(tpe.sym, i)
              }
            case _ => ()
          }

        case t: DefTree if t.symbol != NoSymbol =>
          addDefinition(t)
        case t: RefTree =>
          addReference(t.symbol, t)
        case t: TypeTree =>
          
          def handleType(typ: Type): Unit = typ match {
            case RefinedType(parents, _) =>
              parents foreach handleType
            case TypeRef(_, sym, _) =>
              addReference(sym, t)
            case _ => ()
          }
          
          handleType(t.tpe)
          
        case _ => ()
      }
    }
  }
}
