package scala.tools.refactoring.sourcegen

trait PrettyPrinter {

  val global: scala.tools.nsc.interactive.Global
  import global._
  
  import Transformations._
  
  def prettyPrintTree(traverse: Transformation[Tree, String], t: Tree): String = { 
    
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
          
      case PackageDef(pid, stats) =>
        pid.print(before = "package ", after = "\n") + stats.print(separator = "\n")
        
      case ClassDef(mods, name, tparams, impl) =>
        //mods.annotations map traverse
        //tparams map traverse
        
        "class "+ name + impl.print()
        
  //    case ModuleDef(mods, name, impl) =>
  //      mods.annotations map traverse
  //      traverse(impl)
        
      case t @ ValDef(mods, name, tpt, rhs) if mods.isArgument =>
        //mods.annotations map traverse
        name.toString + tpt.print(before = ": ") + rhs.print(before = " = ")
        
      case t @ ValDef(mods, name, tpt, rhs) =>
        //mods.annotations map traverse
        "val " + name.toString + tpt.print(before = ": ") + rhs.print(before = " = ")
        
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        //mods.annotations map traverse
        val tparams_ = tparams print (before = "[", after = "]", separator = ", ")
        val params = vparamss map (_ print (before = "(", after = ")", separator = ", ")) mkString ""
        "def "+ name + tparams_ + params + tpt.print(before = ": ") + rhs.print(before = " = ")
        
      case TypeDef(mods, name, tparams, rhs) =>
        //mods.annotations map traverse
        //tparams map traverse
        //traverse(rhs)
        name.toString
        
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
        
  //    case Annotated(annot, arg) =>
  //      traverse(annot)
  //      traverse(arg)
        
      case Template(parents, self, Nil) =>
        parents.print() //+
        //self.print()
        
      case Template(parents, self, body) =>
        parents.print() +
        //self.print() +
        body.print(before = " {\n", separator = "\n", after = "\n}\n")

      case Block(stats, expr) =>
        val all = stats ::: expr :: Nil
        if(all.size > 1) {
          all print ( before = "{\n", separator = "\n", after = "\n}")
        } else {
          all.head.print()
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
  //      
  //    case Function(vparams, body) =>
  //      vparams map traverse
  //      traverse(body)
  //      
  //    case Assign(lhs, rhs) =>
  //      traverse(lhs)
  //      traverse(rhs)
  //      
  //    case If(cond, thenp, elsep) =>
  //      traverse(cond)
  //      traverse(thenp)
  //      traverse(elsep)
  //      
  //    case Match(selector, cases) =>
  //      traverse(selector)
  //      cases map traverse
  //      
  //    case Return(expr) =>
  //      traverse(expr)
  //      
  //    case Try(block, catches, finalizer) =>
  //      traverse(block)
  //      catches map traverse
  //      traverse(finalizer)
  //      
  //    case Throw(expr) =>
  //      traverse(expr)
  //      
  //    case New(tpt) =>
  //      traverse(tpt)
  //      
  //    case Typed(expr, tpt) =>
  //      traverse(expr)
  //      traverse(tpt)
  //      
  //    case TypeApply(fun, args) =>
  //      traverse(fun)
  //      args map traverse
        
      case Apply(fun, args) =>
        fun.print() + args.print(before = "(", after = ")", separator = ", ")
        
  //    case ApplyDynamic(qual, args) =>
  //      traverse(qual)
  //      args map traverse
  //      
  //    case Super(_, _) =>
  //      ;
  //      
  //    case This(_) =>
  //      ;
        
      case t @ Select(qualifier, selector) =>
        qualifier.print(after = ".") + t.symbol.nameString
        
      case Ident(name) =>
        name.toString
        
      case lit: Literal =>
        lit.toString
        
      case tree: TypeTree => 
        tree.tpe match {
          case tpe if tpe == EmptyTree.tpe => ""
          case tpe: ConstantType => tpe.underlying.toString
          case r @ RefinedType(parents, _) =>
            parents map {
              case NamedType(name, _) => name.toString
            } mkString
          case _ => 
            tree.tpe.toString
        }
        
  //    case SingletonTypeTree(ref) =>
  //      traverse(ref)
  //      
  //    case SelectFromTypeTree(qualifier, selector) =>
  //      traverse(qualifier)
  //      
  //    case CompoundTypeTree(templ) =>
  //      traverse(templ)
  //      
  //    case AppliedTypeTree(tpt, args) =>
  //      traverse(tpt)
  //      args map traverse
  //      
  //    case TypeBoundsTree(lo, hi) =>
  //      traverse(lo)
  //      traverse(hi)
  //      
  //    case ExistentialTypeTree(tpt, whereClauses) =>
  //      traverse(tpt)
  //      whereClauses map traverse
  //      
  //    case SelectFromArray(qualifier, selector, erasure) =>
  //      traverse(qualifier)
      
      case t: Tree => 
        "«?"+ t.getClass.getSimpleName +"?»"
    } 
  }
  
}