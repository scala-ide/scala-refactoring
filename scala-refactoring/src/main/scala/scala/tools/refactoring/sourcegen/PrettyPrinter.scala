/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import tools.nsc.symtab.{Flags, Names, Symbols}

trait PrettyPrinter extends AbstractPrinter {
  
  this: common.PimpedTrees with common.Tracing with Indentations =>
  
  import global._
  
  def print(t: Tree, ind: Indentation, changeSet: ChangeSet): Fragment = context("pretty print tree "+ t.getClass.getSimpleName) { 
    
    object PrintOverloads {
      
      def p(tree: Tree, before: Requisite, after: Requisite): Fragment =
        printSingleTree(t, tree, ind, changeSet, false, before, after)
      
      def p(tree: Tree, before: Requisite): Fragment =
        printSingleTree(t, tree, ind, changeSet, false, before, NoRequisite)
      
      def p(tree: Tree): Fragment =
        printSingleTree(t, tree, ind, changeSet, false, NoRequisite, NoRequisite)
        
      def p(ts: List[Tree], separator: Requisite): Fragment =
        printManyTrees(t, ts, ind, changeSet, false, separator, NoRequisite, NoRequisite)
        
      def p(ts: List[Tree]): Fragment =
        printManyTrees(t, ts, ind, changeSet, false, NoRequisite, NoRequisite, NoRequisite)
      
      def p(ts: List[Tree], separator: Requisite, before: Requisite, after: Requisite): Fragment =    
        printManyTrees(t, ts, ind, changeSet, false, separator, before, after)
      
      def printIndented(tree: Tree, before: Requisite, after: Requisite) =
        printSingleTree(t, tree, ind.incrementDefault, changeSet, true, before, after)
        
      def printIndented(ts: List[Tree], before: Requisite, separator: Requisite, after: Requisite) =
        printManyTrees(t, ts, ind.incrementDefault, changeSet, true, separator = separator, before = before, after = after)
    }
    
    import PrintOverloads._
    
    implicit def stringToRequisite(regex: String) = Requisite.allowSurroundingWhitespace(regex)
    
    def newline = Requisite.newline(ind.current)
    def indentedNewline = Requisite.newline(ind.incrementDefault.current)
    
    trace("current indentation set to %s", ind.current)
    
    val code: Fragment = t match {
    
      case EmptyTree =>
        EmptyFragment
          
      case PackageDef(pid, stats) if pid.name.toString == "<empty>" =>
        p(stats, separator = newline)
        
      case PackageDef(pid, stats) =>
        p(pid, before = "package ", after = newline) ++ p(stats, separator = newline)
      
      case ClassDef(m @ ModifierTree(mods), name, tparams, impl) =>
        //mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        
        Fragment(mods_ + (if(m.isTrait)
          "" // "trait" is a modifier
        else
          "class ") + name) ++ p(tparams, before = "\\[", separator = ", ", after = "\\]") ++ p(impl)
        
      case ModuleDef(ModifierTree(mods), name, impl) =>
//        mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        Fragment(mods_ + "object " + name) ++ p(impl)
        
      case t @ ValDef(ModifierTree(mods), name, tpt, rhs)  =>
        //mods.annotations map traverse
        val mods_ = {
          val existingMods = mods map (m => m.nameString + " ") mkString ""
          if(!t.symbol.isMutable && t.needsKeyword && !existingMods.contains("val")) {
            existingMods + "val "
          } else {
            existingMods 
          }
        }
        
        val valName = if(t.symbol.isThisSym && name.toString == "_") { // this: ... =>
          "this"
        } else name.toString.trim
                
        val r = p(rhs, before = " = ")
        val x = Fragment(mods_ + valName) ++ p(tpt, before = ": ") ++ r
        x
        
      case t @ DefDef(ModifierTree(mods), name, tparams, vparamss, tpt, _) =>
        //mods.annotations map traverse
        import Requisite._
        
        val mods_ = {
          val existingMods = mods map (m => m.nameString + " ") mkString ""
          if(t.mods.hasFlag(Flags.STABLE)&& !existingMods.contains("val")) {
            existingMods + "val "
          } else {
            existingMods 
          }
        }
        
        val tparams_ = p(tparams, before = "\\[", after = anywhere("]"), separator = ", ")
        val params = Layout(vparamss map (vparams => p(vparams, before = "\\(", after = "\\)", separator = (allowSurroundingWhitespace(",") ++ Blank))) mkString "")
        val rhs = if(t.rhs == EmptyTree && !t.symbol.isDeferred) {
          Fragment(" {\n"+ ind.current +"}")
        } else {
          p(t.rhs, before = " = ")
        } 
        
        Fragment(mods_ + t.nameString) ++ tparams_ ++ params ++ p(tpt, before = ": ") ++ rhs
        
      case t @ TypeDef(ModifierTree(Nil), _, Nil, EmptyTree) if t.symbol.isSynthetic =>
        Fragment("[_]")
        
      case TypeDef(ModifierTree(mods), name, tparams, rhs) =>
        //mods.annotations map traverse
        val mods_ = mods map (m => m.nameString + " ") mkString ""
        val tparams_ = p(tparams, before = "\\[", after = "\\]", separator = ", ")
        val rhs_ = rhs match {
          case rhs: TypeTree if rhs.original.isInstanceOf[TypeBoundsTree] => p(rhs, before = " ")
          case _ => p(rhs, before = " = ")
        }
        Fragment(mods_ + t.nameString) ++ tparams_ ++ rhs_
        
      case label @ LabelDef(_, _, Block(stats, If(cond, _, _))) =>
        Layout("do ") ++ p(stats) ++ Fragment(" while") ++ Layout("(") ++ p(cond) ++ Layout(")")
        
      case label @ LabelDef(name, params, If(cond, Block((body: Block) :: Nil, _), _)) =>
        Fragment(label.nameString) ++ Layout("(") ++ p(cond) ++ Layout(")") ++ p(body)
        
      case label @ LabelDef(name, params, If(cond, then, _)) =>
        Fragment(label.nameString) ++ Layout("(") ++ p(cond) ++ Layout(")") ++ p(then)
        
      case Import(expr, selectors) =>
        
        def renames(s: ImportSelector) = s.rename != null && s.name != s.rename
        val needsBraces = selectors.size > 1 || selectors.exists(renames)
        
        val ss = (selectors map { s =>
          if(renames(s))
            s.name.toString + " => " + s.rename.toString  
          else
            s.name.toString
        } mkString ", ")
        
        Layout("import ") ++ p(expr) ++ "." ++ Fragment(if(needsBraces) "{" + ss + "}" else ss)
        
      case tpl @ TemplateExtractor(params, earlyBody, parents, self, body) =>
                
        val R = Requisite.allowSurroundingWhitespace _
        
        val sup = if(earlyBody.isEmpty) {
          parents match {
            case Nil => EmptyFragment
            case parent :: traits => 
              val superclass = {
                if(tpl.symbol == NoSymbol)
                  p(parent)
                else
                  p(parent, before = " extends ")
              }
              superclass ++ p(traits, before = " with ", separator = " with ", after = NoRequisite)
          }
        } else {
          printIndented(earlyBody, before = R(" extends \\{") ++ 
              indentedNewline, after = newline ++ "\\}", separator = indentedNewline) ++
              p(parents, before = " with ", separator = " with ", after = NoRequisite)
        }
        
        val self_ = (self, body) match {
          case (EmptyTree, body) => 
            printIndented(body, before = R(" \\{") ++ indentedNewline, separator = indentedNewline, after = newline ++ "\\}")
          case (self, Nil) => 
            printIndented(self, before = R(" \\{") ++ indentedNewline, after = R(" =>") ++ newline ++ "\\}")
          case (self, body) => 
            printIndented(self, before = R(" \\{") ++ indentedNewline, after = " =>") ++ 
            printIndented(body, before = indentedNewline, separator = indentedNewline, after = newline ++ "\\}")
        }
        
                
        val x = p(params, before = "\\(", separator = ", ", after = "\\)")
        x ++ sup ++ self_

      case BlockExtractor(stats) =>
        
        def printWithEnclosing = printIndented(stats, before = Requisite.allowSurroundingWhitespace("\\{") ++ indentedNewline, separator = indentedNewline, after = newline ++ ( "\\}"))
        
        // FIXME don't code when tired..
        
        if(stats.size > 1 && !stats.head.hasExistingCode && stats.tail.exists(_.hasExistingCode)) {
          
          val firstWithExistingCode = stats.find(_.hasExistingCode) get
          val printed = p(firstWithExistingCode)
          if(printed.leading.matches("(?ms).*\\{.*")) {
            
            val ExtractOpeningBrace = "(?ms)(.*\\{.*)(\n.*)".r
            val ExtractOpeningBrace(leading, rest) = printed.leading.asText
            
            val printedStats = stats map { 
              case tree if tree == firstWithExistingCode =>
                Fragment(Layout(rest), printed.center, printed.trailing)
              case tree =>
                printIndented(tree, before = NoRequisite, after = NoRequisite)
            }
            
            Layout(leading) ++ indentedNewline ++ printedStats.foldLeft(EmptyFragment: Fragment)(_ ++ _)
            
          } else {
            printWithEnclosing
          }
        } else 
          printWithEnclosing

      case CaseDef(pat, guard, body) =>
        Layout("case ") ++ p(pat) ++ p(guard, before = " if ") ++ p(body, before = " => ")
        
      case Alternative(trees) =>
        p(trees, separator = " | ")
        
      case Star(elem) =>
        p(elem) ++ Layout("*")
        
      case Bind(name, body: Typed) =>
        Layout(name.toString) ++ p(body, before = ": ")
        
      case Bind(name, body: Bind) =>
        Layout(name.toString) ++ p(body, before = " @ \\(", after = "\\)")
        
      case Bind(name, body) =>
        Layout(name.toString) ++ p(body, before = " @ ")
        
      case UnApply(fun, args) =>
        p(fun) ++ p(args, before = "\\(", separator = ", ", after = "\\)")
        
  //    case ArrayValue(elemtpt, trees) =>
  //      traverse(elemtpt)
  //      trees map traverse
        
      case Function(vparam :: Nil, body) if !keepTree(vparam.tpt) =>
        p(vparam, before = "", after = " => ") ++ p(body)
        
      case Function(vparams, body) =>
        p(vparams, before = "\\(", separator = ", ", after = "\\) => ") ++ p(body)
        
      case Assign(lhs, rhs) =>
        p(lhs) ++ " = " ++ p(rhs)
        
      case If(cond, thenp, elsep) =>
        p(cond, before = "if \\(", after = "\\)") ++ 
        printIndented(thenp, before = " ", after = NoRequisite) ++ 
        printSingleTree(t, elsep, ind, changeSet, indent = true, before = " else ", after = NoRequisite)
        
      case Match(selector, cases) =>
        p(selector, before = NoRequisite, after = " match ") ++ printIndented(cases, before = Requisite.allowSurroundingWhitespace("\\{") ++ indentedNewline, separator = indentedNewline, after = newline ++ "\\}")
      
      case Return(expr) =>
        Layout("return ") ++ p(expr)
        
      case Try(block, catches, finalizer) =>        
        
        val _block = if(block.isInstanceOf[Block]) {
          p(block, before = Requisite.allowSurroundingWhitespace("try "))
        } else {
          printIndented(block,before = Requisite.allowSurroundingWhitespace("try \\{") ++ indentedNewline, after = newline ++ "\\}")
        }
        
        val _catches = printIndented(catches, before = Requisite.allowSurroundingWhitespace(" catch \\{") ++ indentedNewline, separator = indentedNewline, after = newline ++ "\\}")
        
        val _finalizer = finalizer match {
          case EmptyTree => EmptyFragment
          case block: Block => p(block, before = Requisite.allowSurroundingWhitespace(" finally "))
          case _ => 
            printIndented(finalizer, before = Requisite.allowSurroundingWhitespace(" finally \\{") ++ indentedNewline, after = newline ++ "\\}")
        }
        
        _block ++ _catches ++ _finalizer
//XXX create a "printBlock"
        
      case Throw(expr) =>
        Layout("throw ") ++ p(expr)
        
      case New(tpt) =>
        Layout("new ") ++ p(tpt)
        
      case Typed(expr, tpt) =>
        p(expr) ++ p(tpt)
        
      case TypeApply(Select(Select(ths: This, selector), _), _) => 
        Fragment(selector.toString)
        
      case TypeApply(fun, args) =>
        p(fun) ++ p(args, before= "\\[", separator = Requisite.Blank,  after = "\\]")
        
      case Apply(fun, args @ (Function(_, _: Match) :: _)) =>
        p(fun) ++ p(args, before = " ", separator = NoRequisite, after = NoRequisite)
        
      case t @ Apply(fun: Select, args @ ((arg1: Apply) :: _)) if fun.name.toString endsWith "_$eq" =>
        p(fun) ++ " = " ++ p(args)
        
      case Apply(fun: Select, arg :: Nil) if fun.name.toString endsWith "_$eq" =>
        p(fun) ++ " = " ++ p(arg)
        
      case Apply(fun: Select, args) if fun.name.toString endsWith "_$eq" =>
        p(fun) ++ " = " ++ p(args, before = "\\(", after = "\\)", separator = ", ")
        
      case Apply(fun, (arg @ Ident(name)) :: Nil) if name.toString == "_" =>
        p(fun) ++ p(arg)
        
      case Apply(fun, args) =>
        val _1 = p(fun)
        val _2 = p(args, before = "\\(", after = "\\)", separator = ", ")
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
        
      case t @ Select(qualifier, name) if name.toString == "<init>" => 
        p(qualifier)
        
      case t @ Select(qualifier: This, selector) if qualifier.qual.toString == "immutable" =>
        Fragment(t.symbol.nameString)
        
      case t @ Select(qualifier, selector) if (selector.toString == "unapply" || selector.toString == "unapplySeq") =>
        p(qualifier)
        
      case t @ Select(qualifier, selector) =>
        p(qualifier, before = NoRequisite, after = ".") ++ Fragment(t.nameString)
        
      case t: Ident =>
        if (t.symbol.isSynthetic && t.name.toString.contains("$"))
          Fragment("_")
        else  
          Fragment(t.name.toString)
        
      case lit: Literal if lit.value.tag == StringTag =>
        Fragment("\""+ lit.value.stringValue +"\"")
          
      case lit: Literal =>
        Fragment(lit.value.stringValue)
        
      case tree: TypeTree =>
        
        Fragment(tree.tpe match {
          case tpe if tpe == EmptyTree.tpe => ""
          case tpe: ConstantType => tpe.underlying.toString
          case tpe: TypeRef if tree.original != null && tpe.sym.nameString.matches("Tuple\\d+") => 
            tpe.toString
          case tpe if tree.original != null && !tpe.isInstanceOf[TypeRef]=> 
            p(tree.original).asText
          case r @ RefinedType(parents, _) =>
            parents map {
              case NamedType(name, _)      => name.toString
              case t @ TypeRef(pre, sym, args) => t.toString
              case RefinedType(parents, _) => parents mkString " with "
              case t => throw new Exception("Unhandled type "+ t.getClass.getSimpleName)
            } mkString " with "
          case tpe: TypeRef =>
            tpe.toString
          case tpe => 
            tpe.toString
        })
        
      case SingletonTypeTree(ref) =>
        p(ref) ++ Layout(".type")
        
  //    case SelectFromTypeTree(qualifier, selector) =>
  //      traverse(qualifier)
        
      case CompoundTypeTree(templ) =>
        p(templ)
        
      case AppliedTypeTree(tpt, args) =>
        p(tpt) ++ p(args, before = "\\[", separator = ", ", after = "\\]")
        
      case TypeBoundsTree(lo, hi) =>
        p(lo, before = ">: ", after = " ") ++ p(hi, before = "<: ")
        
      case ExistentialTypeTree(tpt, (t: TypeDef) :: Nil) if t.symbol.isSynthetic => // [_]
        p(tpt) ++ p(t)
        
      case ExistentialTypeTree(tpt, whereClauses) =>
        p(tpt) ++ p(whereClauses, before = " forSome \\{", separator = NoRequisite, after = "\\}")
      
      case t: ModifierTree =>
        Fragment(t.nameString)
        
      case SuperConstructorCall(clazz, args) =>
        p(clazz) ++ p(args, before = "\\(", separator = ", ", after = "\\)")
        
      case t: Tree => 
        Fragment("«?"+ t.getClass.getSimpleName +"?»")
    }
    
    trace("results in %s", code.asText)
    Fragment(code.asText)
  }
}