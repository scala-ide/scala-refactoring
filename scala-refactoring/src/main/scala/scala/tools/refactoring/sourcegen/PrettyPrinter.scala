package scala.tools.refactoring
package sourcegen

import tools.nsc.symtab.{Flags, Names, Symbols}

trait PrettyPrinter extends AbstractPrinter {
  
  this: common.PimpedTrees with common.Tracing =>
  
  import global._
  
  import Transformations._

  def prettyPrintTree(traverse: (Tree, Indentation) => Option[Fragment], t: Tree, ind: Indentation): Fragment = context("pretty print tree"+ t.getClass.getSimpleName) { 
       
    trace("current indentation set to %s", ind.text)
    
    def prnt(tree: Tree, indent: Boolean = false, before: Requisite = NoRequisite, after: Requisite = NoRequisite): Fragment = {

      printSingleTree(t, tree, ind, traverse, indent, before, after)
    }
    
    def prntMany(ts: List[Tree], separator: Requisite = NoRequisite, indent: Boolean = false, before: Requisite = NoRequisite, after: Requisite = NoRequisite): Fragment = {      

      printManyTrees(t, ts, ind, traverse, indent, separator, before, after)
    }
  
    implicit def stringToRequisite(regex: String) = Requisite.allowSurroundingWhitespace(regex)
    
    def newline = Requisite.newline(ind.text)
    
    val code: Fragment = t match {
    
      case EmptyTree =>
        EmptyFragment
          
      case PackageDef(pid, stats) if pid.name.toString == "<empty>" =>
        prntMany(stats, separator = newline)
        
      case PackageDef(pid, stats) =>
        val _1 = prnt(pid, before = "package ", after = newline)
        val _2 = prntMany(stats, separator = newline)
        val r = _1 ++ _2
        r
      
      case ClassDef(m @ ModifierTree(mods), name, tparams, impl) =>
        //mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        
        Fragment(mods_ + (if(m.isTrait)
          "" // "trait" is a modifier
        else
          "class ") + name) ++ prntMany(tparams, before = "\\[", separator = ", ", after = "\\]") ++ prnt(impl)
        
      case ModuleDef(ModifierTree(mods), name, impl) =>
//        mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        Fragment(mods_ + "object " + name) ++ prnt(impl)
        
      case t @ ValDef(m @ ModifierTree(mods), name, tpt, rhs)  =>
        //mods.annotations map traverse
        var mods_ = mods map (m => m.nameString + " ") mkString ""
        
        if(t.needsKeyword && !mods_.contains("val")) {
          mods_ = mods_ + "val "
        }
                
        val r = prnt(rhs, before = " = ")
        val x = Fragment(mods_ + name.toString.trim) ++ prnt(tpt, before = ": ") ++ r
        x
        
      case t @ DefDef(ModifierTree(mods), name, tparams, vparamss, tpt, _) =>
        //mods.annotations map traverse
        var mods_ = mods map (m => m.nameString + " ") mkString ""
        
        if(t.mods.hasFlag(Flags.STABLE) && !mods_.contains("val")) {
          mods_ = mods_ + "val "
        }
        
        val tparams_ = prntMany(tparams, before = "\\[", after = "\\]", separator = ", ")
        val params = vparamss map (p => prntMany(p, before = "\\(", after = "\\)", separator = ", ")) mkString ""
        val rhs = if(t.rhs == EmptyTree && !t.symbol.isDeferred) {
          Fragment(" {\n"+ ind.text +"}")
        } else {
          prnt(t.rhs, before = " = ")
        }
        
        Fragment(mods_ + t.nameString) ++ tparams_ ++ params ++ prnt(tpt, before = ": ") ++ rhs
        
      case TypeDef(ModifierTree(mods), name, tparams, rhs) =>
        //mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        val tparams_ = prntMany(tparams, before = "\\[", after = "\\]", separator = ", ")
        val rhs_ = rhs match {
          case rhs: TypeTree if rhs.original.isInstanceOf[TypeBoundsTree] => prnt(rhs, before = " ")
          case _ => prnt(rhs, before = " = ")
        }
        Fragment(mods_ + name) ++ tparams_ ++ rhs_
        
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
        
        Layout("import ") ++ prnt(expr) ++ "." ++ Fragment(if(needsBraces) "{" + ss + "}" else ss)
        
      case TemplateExtractor(params, earlyBody, parents, self, body) =>
                
        val R = Requisite.allowSurroundingWhitespace _
        val nl = newline
        
        val sup = if(earlyBody.isEmpty) {
          parents match {
            case Nil => EmptyFragment
            case superclass :: traits => 
              prnt(superclass, before = " extends ") ++ prntMany(traits, before = " with ", separator = " with ")
          }
        } else {
          ind.default {
            prntMany(earlyBody, indent = true, before = R(" extends \\{") ++ newline, after = nl ++ "\\}", separator = newline) ++
            prntMany(parents, before = " with ", separator = " with ")
          }
        }
        
        val self_ = ind.default {
          (self, body) match {
            case (EmptyTree, body) => 
              prntMany(body, indent = true, before = R(" \\{") ++ newline, separator = newline, after = nl ++ "\\}")
            case (self, Nil) => 
              prnt(self, indent = true, before = R(" \\{") ++ newline, after = R(" =>") ++ nl ++ "\\}")
            case (self, body) => 
              prnt(self, indent = true, before = R(" \\{") ++ newline, after = " =>") ++ 
              prntMany(body, indent = true, before = newline, separator = newline, after = nl ++ "\\}")
          }
        }
                
        val x = prntMany(params, before = "\\(", separator = ", ", after = "\\)")
        x ++ sup ++ self_

      case BlockExtractor(stats) =>
        // pretty printing always creates braces around the block statements, even if there is only one
        val nl = newline
        println("printing block with parent indentation: «"+ ind.text +"»")
        ind.default {
          prntMany(stats, indent = true, before = Requisite.allowSurroundingWhitespace("\\{") ++ newline, separator = newline, after = nl ++ ( "\\}"))
        }
        
      case CaseDef(pat, guard, body) =>
        Layout("case ") ++ prnt(pat) ++ prnt(guard, before = " if ") ++ prnt(body, before = " => ")
        
      case Alternative(trees) =>
        prntMany(trees, separator = " | ")
        
      case Star(elem) =>
        prnt(elem) ++ Layout("*")
        
        
      case Bind(name, body: Typed) =>
        Fragment(name.toString) ++ prnt(body, before = ": ")
        
      case Bind(name, body: Bind) =>
        Fragment(name.toString) ++ prnt(body, before = " @ \\(", after = "\\)")
        
      case Bind(name, body) =>
        Fragment(name.toString) ++ prnt(body, before = " @ ")
        
      case UnApply(fun, args) =>
        prnt(fun) ++ prntMany(args, before = "\\(", separator = ", ", after = "\\)")
        
  //    case ArrayValue(elemtpt, trees) =>
  //      traverse(elemtpt)
  //      trees map traverse
        
      case Function(vparams, body) =>
        prntMany(vparams, before = "\\(", separator = ", ", after = "\\) => ") ++ prnt(body)
        
      case Assign(lhs, rhs) =>
        prnt(lhs) ++ " = " ++ prnt(rhs)
        
      case If(cond, thenp, elsep) =>
        prnt(cond, before = "if \\(", after = "\\)") ++ prnt(thenp, indent = true, before = " ") ++ prnt(elsep, indent = true, before = " else ")
        
      case Match(selector, cases) =>
        val nl = newline
        ind.default {
          prnt(selector, after = " match ") ++ prntMany(cases, indent = true, before = Requisite.allowSurroundingWhitespace("\\{") ++ newline, separator = newline, after = nl ++ "\\}")
        }
      case Return(expr) =>
        Layout("return ") ++ prnt(expr)
        
      case Try(block, catches, finalizer) =>        
        val nl = newline
        
        val _block = if(block.isInstanceOf[Block]) {
          prnt(block, before = Requisite.allowSurroundingWhitespace("try "))
        } else {
          ind.default {
            prnt(block, indent = true, before = Requisite.allowSurroundingWhitespace("try \\{") ++ newline, after = nl ++ "\\}")
          }
        }
        
        val _catches = ind.default {
          prntMany(catches, indent = true, before = Requisite.allowSurroundingWhitespace(" catch \\{") ++ newline, separator = newline, after = nl ++ "\\}")
        }
        
        val _finalizer = finalizer match {
          case EmptyTree => EmptyFragment
          case block: Block => prnt(block, before = Requisite.allowSurroundingWhitespace(" finally "))
          case _ => ind.default {
            prnt(finalizer, indent = true, before = Requisite.allowSurroundingWhitespace(" finally \\{") ++ newline, after = nl ++ "\\}")
          }
        }
        
        _block ++ _catches ++ _finalizer
//XXX create a "printBlock"
        
  //    case Throw(expr) =>
  //      traverse(expr)
        
      case New(tpt) =>
        Layout("new ") ++ prnt(tpt)
        
      case Typed(expr, tpt) =>
        prnt(expr) ++ prnt(tpt)
        
      case TypeApply(Select(Select(ths: This, selector), _), _) => 
        Fragment(selector.toString)
        
      case TypeApply(fun, args) =>
        prnt(fun) ++ prntMany(args)
        
      case Apply(fun, args @ (Function(_, _: Match) :: _)) =>
        prnt(fun) ++ prntMany(args, before = " ")
        
      case Apply(fun: Select, arg :: Nil) if fun.name.toString endsWith "_$eq" =>
        prnt(fun) ++ " = " ++ prnt(arg)
        
      case Apply(fun: Select, args) if fun.name.toString endsWith "_$eq" =>
        prnt(fun) ++ " = " ++ prntMany(args, before = "\\(", after = "\\)", separator = ", ")
        
      case Apply(fun, args) =>
        val _1 = prnt(fun)
        val _2 = prntMany(args, before = "\\(", after = "\\)", separator = ", ")
        val _3 = _1 ++ _2
        _3
        
  //    case ApplyDynamic(qual, args) =>
  //      traverse(qual)
  //      args map traverse
        
      case Super(qual, mix) =>
        val q = if(qual.toString == "") "" else qual +"."
        val m = if(mix.toString == "") "" else "["+ mix + "]"
        Fragment(q +"super"+ m)
        
      case This(qual) =>
        Fragment((if(qual.toString == "") "" else qual +".") + "this")
        
      case t @ Select(qualifier: This, selector) if qualifier.qual.toString == "immutable" =>
        Fragment(t.symbol.nameString)
        
      case t @ Select(qualifier, selector) if (selector.toString == "unapply" || selector.toString == "unapplySeq") =>
        prnt(qualifier)
        
      case t @ Select(qualifier, selector) =>
        prnt(qualifier, after = ".") ++ Fragment(t.nameString)
        
      case t: Ident =>
        if (t.symbol.isSynthetic && t.name.toString.contains("$"))
          Fragment("_")
        else  
          Fragment(t.name.toString)
        
      case lit: Literal =>
        Fragment(lit.toString)
        
      case tree: TypeTree =>
        
        Fragment(tree.tpe match {
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
            prnt(tree.original).asText
          case tpe => 
            tpe.toString
        })
        
  //    case SingletonTypeTree(ref) =>
  //      traverse(ref)
  //      
  //    case SelectFromTypeTree(qualifier, selector) =>
  //      traverse(qualifier)
  //      
  //    case CompoundTypeTree(templ) =>
  //      traverse(templ)
        
      case AppliedTypeTree(tpt, args) =>
        prnt(tpt) ++ prntMany(args, before = "\\[", separator = ", ", after = "\\]")
        
      case TypeBoundsTree(lo, hi) =>
        prnt(lo, before = ">: ", after = " ") ++ prnt(hi, before = "<: ")
        
  //    case ExistentialTypeTree(tpt, whereClauses) =>
  //      traverse(tpt)
  //      whereClauses map traverse
  //      
  //    case SelectFromArray(qualifier, selector, erasure) =>
  //      traverse(qualifier)
      
      case t: ModifierTree =>
        Fragment(t.nameString)
        
      case SuperConstructorCall(clazz, args) =>
        prnt(clazz) ++ prntMany(args, before = "\\(", separator = ", ", after = "\\)")
        
      case t: Tree => 
        Fragment("«?"+ t.getClass.getSimpleName +"?»")
    } 
    
    trace("results in %s", code.asText)
    Fragment(code.asText)
  }
}