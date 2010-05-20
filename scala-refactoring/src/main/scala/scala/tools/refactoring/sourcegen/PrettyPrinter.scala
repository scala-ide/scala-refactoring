package scala.tools.refactoring
package sourcegen

import tools.nsc.symtab.{Flags, Names, Symbols}

trait PrettyPrinter {
  
  self: common.PimpedTrees with common.Tracing =>

  val global: scala.tools.nsc.interactive.Global
  import global._
  
  import Transformations._
  
  private class PrintFunctionsForTree(ts: List[Tree], traverse: (Tree, Indentation) => Option[Fragment], ind: Indentation) {
    
    def print(before: String = "", after: String = "", separator: String = "") = (ts map (traverse(_, ind))) flatten match {
      case Nil => ""
      case xs => before + (xs mkString separator) + after
    }
    
    def printIndented(before: () => String = () => "", after: () => String = () => "", separator: () => String = () => "") = {
      
      ind.default {
        (ts map (traverse(_, ind))) flatten match {
          case Nil => ""
          case xs => before() + (xs mkString separator())
        }
      } match {
        case "" => ""
        case s => s + after()
      }
    }  
  }
  
  def prettyPrintTree(traverse: (Tree, Indentation) => Option[Fragment], t: Tree, ind: Indentation): Fragment = context("pretty print tree"+ t.getClass.getSimpleName) { 
       
    trace("base indentation is %s", ind.text)
    
    implicit def additionalMethodsForTreePrinting(t: Tree) = new PrintFunctionsForTree(t :: Nil, traverse, ind)
    implicit def additionalMethodsForTreeListPrinting(ts: List[Tree]) = new PrintFunctionsForTree(ts, traverse, ind)
    
    def newline = "\n" + ind.text
    
    val code = t match {
    
      case EmptyTree =>
        ""
          
      case PackageDef(pid, stats) if pid.name.toString == "<empty>" =>
        stats.print(separator = newline)
        
      case PackageDef(pid, stats) =>
        pid.print(before = "package ", after = newline) + stats.print(separator = newline)
        
      case ClassDef(m @ ModifierTree(mods), name, tparams, impl) =>
        //mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        
        mods_ + (if(m.isTrait)
          "" // "trait" is a modifier
        else
          "class ") + name + tparams.print(before = "[", separator = ", ", after = "]") + impl.print()
        
      case ModuleDef(ModifierTree(mods), name, impl) =>
//        mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        mods_ + "object " + name + impl.print()
        
      case t @ ValDef(m @ ModifierTree(mods), name, tpt, rhs)  =>
        //mods.annotations map traverse
        var mods_ = mods map (m => m.nameString + " ") mkString ""
        
        if(t.needsKeyword && !mods_.contains("val")) {
          mods_ = mods_ + "val "
        }
                
        mods_ + name.toString.trim + tpt.print(before = ": ") + rhs.print(before = " = ")
        
      case t @ DefDef(ModifierTree(mods), name, tparams, vparamss, tpt, _) =>
        //mods.annotations map traverse
        var mods_ = mods map (m => m.nameString + " ") mkString ""
        
        if(t.mods.hasFlag(Flags.STABLE) && !mods_.contains("val")) {
          mods_ = mods_ + "val "
        }
        
        val tparams_ = tparams print (before = "[", after = "]", separator = ", ")
        val params = vparamss map (_ print (before = "(", after = ")", separator = ", ")) mkString ""
        val rhs = if(t.rhs == EmptyTree && !t.symbol.isDeferred) {
          " {"+ newline +"}"
        } else {
          t.rhs.print(before = " = ")
        }
        mods_ + t.nameString + tparams_ + params + tpt.print(before = ": ") + rhs
        
      case TypeDef(ModifierTree(mods), name, tparams, rhs) =>
        //mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        val tparams_ = tparams.print(before = "[", after = "]", separator = ", ")
        val rhs_ = rhs match {
          case rhs: TypeTree if rhs.original.isInstanceOf[TypeBoundsTree] => rhs.print(before = " ")
          case _ => rhs.print(before = " = ")
        }
        mods_ + name + tparams_ + rhs_
        
  //    case LabelDef(name, params, rhs) =>
  //      params map traverse
  //      traverse(rhs)
        
      case Import(expr, selectors) =>
        
        def renames(s: ImportSelector) = s.rename != null && s.name != s.rename
        val needsBraces = selectors.size > 1 || selectors.exists(renames)
        
        val ss = (selectors map { s =>
          if(renames(s))
            s.name.toString + " => " + s.rename.toString  
          else
            s.name.toString
        } mkString ", ")
        
        "import " + expr.print() + "." + (if(needsBraces) "{" + ss + "}" else ss)
        
      case TemplateExtractor(params, earlyBody, parents, self, body) =>
        
        val sup = if(earlyBody.isEmpty) {
          parents match {
            case Nil => ""
            case superclass :: traits => 
              superclass.print(before = " extends ") + 
              traits.print(before = " with ", separator = " with ")
          }
        } else {
          earlyBody.printIndented(before = (() => " extends {"+ newline), after = (() => newline +"}"), separator = newline _) +
          parents.print(before = " with ", separator = " with ")
        }
                
        val self_ = if(self != EmptyTree) {
          self.printIndented(before = (() => " {"+ newline), after = () => " =>") + body.printIndented(before = newline _, separator = newline _) + newline +"}"
        } else {
          body.printIndented(before = (() => " {"+ newline), separator = newline _, after = (() => newline +"}"+ newline))
        }
                
        params.print(before = "(", separator = ", ", after = ")") + sup + self_

        
      case BlockExtractor(stats) =>
        stats match {
          case t :: Nil => t.print()
          case t => t printIndented ( before = (() => "{"+ newline), separator = newline _, after = (() => newline +"}"))
        }
        
      case CaseDef(pat, guard, body) =>
        "case " + pat.print() + guard.print(before = " if ") + body.print(before = " => ")
        
      case Alternative(trees) =>
        trees.print(separator = " | ")
        
  //    case Star(elem) =>
  //      traverse(elem)
        
      case Bind(name, body: Typed) =>
        name + body.print(before = ": ")
        
      case Bind(name, body: Bind) =>
        name + body.print(before = " @ (", after = ")")
        
      case Bind(name, body) =>
        name + body.print(before = " @ ")
        
      case UnApply(fun, args) =>
        fun.print() + args.print(before = "(", separator = ", ", after = ")")
        
  //    case ArrayValue(elemtpt, trees) =>
  //      traverse(elemtpt)
  //      trees map traverse
        
      case Function(vparams, body) =>
        vparams.print(before = "(", separator = ", ", after = ") => ") + body.print()
        
      case Assign(lhs, rhs) =>
        lhs.print() + " = " + rhs.print()
        
      case If(cond, thenp, elsep) =>
        cond.print(before = "if (", after = ")") + thenp.print(before = " ") + elsep.print(before = " else ")
        
      case Match(selector, cases) =>
        selector.print(after = " match ") + cases.printIndented(before = (() => "{"+ newline), separator = newline _, after = (() => newline +"}"))
        
      case Return(expr) =>
        "return " + expr.print()
        
  //    case Try(block, catches, finalizer) =>
  //      traverse(block)
  //      catches map traverse
  //      traverse(finalizer)
  //      
  //    case Throw(expr) =>
  //      traverse(expr)
        
      case New(tpt) =>
        "new " + tpt.print()
        
      case Typed(expr, tpt) =>
        expr.print() + tpt.print()
        
      case TypeApply(fun, args) =>
        fun.print() + args.print()
        
      case Apply(fun, args @ (Function(_, _: Match) :: _)) =>
        fun.print() + args.print(before = " ")
        
      case Apply(fun: Select, arg :: Nil) if fun.name.toString endsWith "_$eq" =>
        fun.print() + " = "+ arg.print()
        
      case Apply(fun: Select, args) if fun.name.toString endsWith "_$eq" =>
        fun.print() + " = "+ args.print(before = "(", after = ")", separator = ", ")
        
      case Apply(fun, args) =>
        fun.print() + args.print(before = "(", after = ")", separator = ", ")
        
  //    case ApplyDynamic(qual, args) =>
  //      traverse(qual)
  //      args map traverse
  //      
  //    case Super(_, _) =>
  //      ;
        
      case t: This =>
        "this"
        
      case t @ Select(qualifier: This, selector) if qualifier.qual.toString == "immutable" =>
        t.symbol.nameString
        
      case t @ Select(qualifier, selector) if selector.toString == "unapply" =>
        qualifier.print()
        
      case t @ Select(qualifier, selector) =>
        qualifier.print(after = ".") + t.nameString
        
      case t: Ident =>
        if (t.symbol.isSynthetic && t.name.toString.contains("$"))
          "_"
        else t.name.toString
        
      case lit: Literal =>
        lit.toString
        
      case tree: TypeTree =>
        
        tree.tpe match {
          case tpe if tpe == EmptyTree.tpe => ""
          case tpe: ConstantType => tpe.underlying.toString
          case r @ RefinedType(_ :: parents, _) =>
            parents map {
              case NamedType(name, _)      => name.toString
              case TypeRef(pre, sym, args) => sym.nameString
              case RefinedType(parents, _) => parents mkString " with "
              case t => throw new Exception("Unhandled type "+ t.getClass.getSimpleName)
            } mkString
          case tpe: TypeRef if tree.original != null && tpe.sym.nameString.matches("Tuple\\d+") => 
            val n = tpe.sym.nameString
            tpe.toString
          case tpe: TypeRef =>
            tpe.toString
          case _ if tree.original != null => 
            tree.original.print()
          case tpe => 
            tpe.toString
        }
        
  //    case SingletonTypeTree(ref) =>
  //      traverse(ref)
  //      
  //    case SelectFromTypeTree(qualifier, selector) =>
  //      traverse(qualifier)
  //      
  //    case CompoundTypeTree(templ) =>
  //      traverse(templ)
        
      case AppliedTypeTree(tpt, args) =>
        tpt.print() + args.print(before = "[", separator = ", ", after = "]")
        
      case TypeBoundsTree(lo, hi) =>
        lo.print(before = ">: ", after = " ") + hi.print(before = "<: ")
        
  //    case ExistentialTypeTree(tpt, whereClauses) =>
  //      traverse(tpt)
  //      whereClauses map traverse
  //      
  //    case SelectFromArray(qualifier, selector, erasure) =>
  //      traverse(qualifier)
      
      case t: ModifierTree =>
        t.nameString
        
      case SuperConstructorCall(clazz, args) =>
        clazz.print() + args.print(before = "(", separator = ", ", after = ")")
        
      case t: Tree => 
        "«?"+ t.getClass.getSimpleName +"?»"
    } 
    
    Fragment(code)
  }
}