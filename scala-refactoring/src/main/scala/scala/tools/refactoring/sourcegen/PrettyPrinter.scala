package scala.tools.refactoring
package sourcegen

import tools.nsc.symtab.{Flags, Names, Symbols}

trait PrettyPrinter {
  
  self: common.PimpedTrees with common.Tracing with common.CustomTrees =>

  val global: scala.tools.nsc.interactive.Global
  import global._
  
  import Transformations._
  
  def prettyPrintTree(traverse: Transformation[Tree, String], t: Tree): String = context("pretty print tree"+ t.getClass.getSimpleName) { 
    
    implicit def treeToPP(t: Tree) = new {
      def print (
          before: String = "",
          f: String => String = (x => x),
          after: String = "") = traverse(t) match {
        case Some(t) => before + f(t) + after
        case None => ""
      }
    }
    
    implicit def treeListToPP(ts: List[Tree]) = new {
      def print (
          before: String = "", 
          f: String => String = (x => x), 
          after: String = "", 
          separator: String = "") = (ts map traverse) flatten match {
        case Nil => ""
        case xs => before + (xs map f mkString separator) + after
      }
    }
    
    t match {
    
      case EmptyTree =>
        ""
          
      case PackageDef(pid, stats) if pid.name.toString == "<empty>" =>
        stats.print(separator = "\n")
        
      case PackageDef(pid, stats) =>
        pid.print(before = "package ", after = "\n") + stats.print(separator = "\n")
        
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
          " {\n}\n"
        } else {
          t.rhs.print(before = " = ")
        }
        mods_ + name + tparams_ + params + tpt.print(before = ": ") + rhs
        
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
  
  //    eliminated by type checker
  //    case Annotated(annot, arg) =>
        
      case TemplateExtractor(params, earlyBody, parents, self, body) =>
        
        val sup = if(earlyBody.isEmpty) {
          parents match {
            case Nil => ""
            case superclass :: traits => 
              superclass.print(before = " extends ") + 
              traits.print(before = " with ", separator = " with ")
          }
        } else {
          earlyBody.print(before = " extends {\n", after = "\n}", separator = "\n") +
          parents.print(before = " with ", separator = " with ")
        }
                
        val self_ = if(self != EmptyTree) {
          self.print(before = " {\n", after = " =>\n") + body.print(separator = "\n") + "}"
        } else {
          body.print(before = " {\n", separator = "\n", after = "\n}\n")
        }
                
        params.print(before = "(", separator = ", ", after = ")") + sup + self_

        
      case BlockExtractor(stats) =>
        stats match {
          case t :: Nil => t.print()
          case t => t print ( before = "{\n", separator = "\n", after = "\n}")
        }
        
  //    case CaseDef(pat, guard, body) =>
  //      traverse(pat)
  //      traverse(guard)
  //      traverse(body)
  //      
  //    case Alternative(trees) =>
  //      trees map traverse
  //      
  //    case Star(elem) =>
  //      traverse(elem)
  //      
  //    case Bind(name, body) =>
  //      traverse(body)
  //      
  //    case UnApply(fun, args) =>
  //      traverse(fun)
  //      args map traverse
  //      
  //    case ArrayValue(elemtpt, trees) =>
  //      traverse(elemtpt)
  //      trees map traverse
        
      case Function(vparams, body) =>
        vparams.print(before = "(", separator = ", ", after = ") => ") + body.print()
        
  //    case Assign(lhs, rhs) =>
  //      traverse(lhs)
  //      traverse(rhs)
        
      case If(cond, thenp, elsep) =>
        cond.print(before = "if (", after = ")") + thenp.print(before = "\n") + elsep.print(before = "\nelse ")
        
  //    case Match(selector, cases) =>
  //      traverse(selector)
  //      cases map traverse
        
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
        
  //    case Typed(expr, tpt) =>
  //      traverse(expr)
  //      traverse(tpt)
        
      case TypeApply(fun, args) =>
        fun.print() + args.print()
        
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
        
      case t @ Select(qualifier, selector) =>
        qualifier.print(after = ".") + t.symbol.nameString
        
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
        
      case SuperConstructorCall(clazz: Tree, args: List[Tree]) =>
        clazz.print() + args.print(before = "(", separator = ", ", after = ")")
        
      case t: Tree => 
        "«?"+ t.getClass.getSimpleName +"?»"
    } 
  }
  
}