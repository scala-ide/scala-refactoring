package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.symtab.Symbols
import scala.collection.mutable.{HashMap, ListBuffer}

trait Indexes {

  val global: scala.tools.nsc.Global  
  import global._
  
  class Index {
    
    private type Refs = ListBuffer[RefTree]
    private type Defs = ListBuffer[DefTree]
  
    private val defs = new HashMap[Symbol, DefTree]
    private val refs = new HashMap[Symbol, Refs]
    private val chld = new HashMap[Symbol, Defs]

    private def dbgSymbol(sym: Symbol) = "Symbol "+ sym.nameString +" ["+ sym.id +"] ("+ sym.pos +")"
    private def dbgTree(t: Tree) = t.getClass.getSimpleName +" "+ t.toString.replaceAll("\n", " ") +" ("+ t.pos +")" 
    
    def debugString() = { 
      
      val ds = defs map {
        case (sym, t) =>
          dbgSymbol(sym) +" is defined by: \n -"+ dbgTree(t)
      } mkString "\n\n"
      
      val rs = refs map {
        case (sym, refs) =>
          dbgSymbol(sym) +" is referenced by: \n -"+ (refs map dbgTree mkString "\n -") 
      } mkString "\n\n"
      
      ds +"\n\n====\n\n"+ rs
    }

    def declaration(s: Symbol): DefTree = defs(s)
    
    def references (s: Symbol) = occurences(s) filterNot(_ == declaration(s))
    
    def occurences(s: Symbol): List[SymTree] = {
      s :: dependentSymbols(s) flatMap { s =>
        declaration(s) :: refs.getOrElse(s, new Refs).toList ::: findInHierarchy(s)
      } distinct
    }
    
    private def dependentSymbols(s: Symbol) = {
      
      def superClassParameters(s: Symbol): List[Symbol] = s match {
        case _ if s != NoSymbol && s.owner.isClass && s.isGetterOrSetter =>
          
          List(declaration(s.owner)) partialMap {
            case ClassDef(_, _, _, Template(_, _, body)) => body partialMap {
              case d @ DefDef(_, _, _, _, _, Block(stats, _)) if d.symbol.isConstructor => stats partialMap {
                case Apply(_, args) => args partialMap {
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
      
      superClassParameters(s) ::: resolveGetterImplRelations(s) filter (_ != NoSymbol)
    }

    private def findInHierarchy(s: Symbol) = s match {
      case s: TermSymbol if s.owner.isClass =>
      
        def allSubClasses(clazz: Symbol) = defs.filter(_._1.ancestors.contains(clazz))
      
        val overrides = allSubClasses(s.owner) map (s overriddenSymbol _._1) filter (_ != NoSymbol) toList
        
        overrides flatMap occurences distinct
      case _ => Nil
    }
    
    def children(s: Symbol) = chld.getOrElse(s, new Defs) toList
   
    def processTree(t: Tree): Unit = t foreach {
      // The standard traverser does not traverse a TypeTree's original:
      case t: TypeTree if t.original != null =>
        processTree(t.original)
      case t: DefTree if t.symbol != NoSymbol => 
        defs += t.symbol → t
        chld.getOrElseUpdate(t.symbol.owner, new Defs) += t
      case t: RefTree =>
        refs.getOrElseUpdate(t.symbol, new Refs) += t
      case _ => ()
    }
  }
}
