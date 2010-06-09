package scala.tools.refactoring
package analysis

import scala.collection.mutable.{HashMap, ListBuffer}

/**
 * A CompilationUnitIndex is a light-weight index that
 * holds all definitions and references in a compilation
 * unit. This index is built with the companion object, 
 * which traverses the whole compilation unit once and
 * then memoizes all relations.
 * 
 * */
trait CompilationUnitIndexes {
  
  val global: scala.tools.nsc.interactive.Global
  
  import global._
  
  trait CompilationUnitIndex {
    def definitions: Map[Symbol, List[DefTree]]
    def references:  Map[Symbol, List[Tree]]
  }
  
  object CompilationUnitIndex {
  
    def apply(tree: Tree) = {
      
      val defs = new HashMap[Symbol, ListBuffer[DefTree]]
      val refs = new HashMap[Symbol, ListBuffer[Tree]]
      
      def processTree(tree: Tree): Unit = {
  
        def addDefinition(t: DefTree) {
          defs.getOrElseUpdate(t.symbol, new ListBuffer[DefTree]) += t
        }
  
        def addReference(s: Symbol, t: Tree) {
          refs.getOrElseUpdate(s, new ListBuffer[Tree]) += t
        }
  
        tree foreach {
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
    
      processTree(tree)
      
      new CompilationUnitIndex {
        val definitions = defs.map {case (k, v) => k → v.toList} toMap
        val references = refs.map {case (k, v) => k → v.toList} toMap
      }
    }
  }
}