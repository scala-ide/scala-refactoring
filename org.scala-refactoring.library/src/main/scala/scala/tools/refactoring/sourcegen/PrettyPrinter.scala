/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import scala.tools.nsc.symtab.Flags
import Requisite.anywhere
import scala.reflect.NameTransformer

trait PrettyPrinter extends TreePrintingTraversals with AbstractPrinter {
  
  outer: common.PimpedTrees with common.CompilerAccess with common.Tracing with Indentations with LayoutHelper =>
  
  import global._
  
  object prettyPrinter extends TreePrinting with PrintingUtils
    with MiscPrinters 
    with MethodCallPrinters
    with WhilePrinters  
    with PatternMatchingPrinters
    with TypePrinters
    with FunctionPrinters 
    with ImportPrinters  
    with PackagePrinters 
    with TryThrowPrinters
    with ClassModulePrinters
    with IfPrinters
    with ValDefDefPrinters
    with SuperPrinters
    with BlockPrinters
    with LiteralPrinters
    
  trait PrintingUtils {
    this: TreePrinting =>
    
    implicit def allowSurroundingWhitespace(s: String) = Requisite.allowSurroundingWhitespace(s)
    
    def newline(implicit ctx: PrintingContext) = Requisite.newline(ctx.ind.current, NL)
    
    def indentedNewline(implicit ctx: PrintingContext) = Requisite.newline(ctx.ind.incrementDefault.current, NL)
    
    def printParameterList(vparamss: List[List[ValDef]], alreadyHasParensInLayout: => Boolean)(implicit ctx: PrintingContext) = vparamss match {
      // no parameter list, not even an empty one
      case Nil => 
        NoLayout
      case vparamss =>
        
        val layout = Layout(
            vparamss map { vparams => 
                pp(vparams, before = "(", after = anywhere(")"), separator = ("," ++ Requisite.Blank))
            } mkString "" // ?
        )
        
        // we want to at least empty parens, the case without any parameters is handled above
        if(layout.asText.isEmpty && !alreadyHasParensInLayout) 
          Layout("()")
        else
          layout
    }
    
    def printTemplate(tpl: Template, printExtends: Boolean)(implicit ctx: PrintingContext) = tpl match {
        case TemplateExtractor(params, earlyBody, parents, self, body) =>

          val sup = if(earlyBody.isEmpty) {
            parents match {
              case Nil => EmptyFragment
              case parent :: traits => 
                val superclass = {
                  if(printExtends)
                    p(parent, before = " extends ")
                  else
                    p(parent)
                }
                superclass ++ pp(traits, before = " with ", separator = " with ")
            }
          } else {
            ppi(earlyBody, before = " extends {" ++ 
                indentedNewline, after = newline ++ "}", separator = indentedNewline) ++
                pp(parents, before = " with ", separator = " with ")
          }
          
          val self_ = (self, body) match {
            case (EmptyTree, body) => 
              ppi(body, before = " {" ++ indentedNewline, separator = indentedNewline, after = newline ++ "}")
            case (self, Nil) => 
              pi(self, before = " {" ++ indentedNewline, after = " =>" ++ newline ++ "}")
            case (self, body) => 
              pi(self, before = " {" ++ indentedNewline, after = " =>") ++ 
              ppi(body, before = indentedNewline, separator = indentedNewline, after = newline ++ "}")
          }
          
          val params_ = printParameterList(params, true)
  
          params_ ++ sup ++ self_
      }
  }
  
  trait WhilePrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def LabelDef(tree: LabelDef, name: Name, params: List[Tree], rhs: Tree)(implicit ctx: PrintingContext) = {
      rhs match {
  
        case Block(stats, If(cond, _, _)) =>
          Layout("do ") ++ pp(stats) ++ Fragment(" while") ++ Layout("(") ++ p(cond) ++ Layout(")")
        
        case If(cond, Block((body: Block) :: Nil, _), _) =>
          Fragment(tree.nameString) ++ Layout("(") ++ p(cond) ++ Layout(")") ++ p(body)
        
        case If(cond, then, _) =>
          Fragment(tree.nameString) ++ Layout("(") ++ p(cond) ++ Layout(")") ++ p(then)
      }
    }
  }
  
  trait PatternMatchingPrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def CaseDef(tree: CaseDef, pat: Tree, guard: Tree, body: Tree)(implicit ctx: PrintingContext) = {
      Layout("case ") ++ p(pat) ++ p(guard, before = " if ") ++ p(body, before = " => ")      
    }
    
    override def Alternative(tree: Alternative, trees: List[Tree])(implicit ctx: PrintingContext) = {
      pp(trees, separator = " | ")
    }
    
    override def Star(tree: Star, elem: Tree)(implicit ctx: PrintingContext) = {
      p(elem) ++ Layout("*")
    }
    
    override def Bind(tree: Bind, name: Name, body: Tree)(implicit ctx: PrintingContext) = {
      body match {
        
        case body: Typed =>
          Layout(name.toString) ++ p(body, before = ": ")
      
        case body: Bind =>
          Layout(name.toString) ++ p(body, before = " @ (", after = ")")
      
        case _ =>
          Layout(name.toString) ++ p(body, before = " @ ")  
      }
    }
    
    override def UnApply(tree: UnApply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext) = {
      p(fun) ++ pp(args, before = "(", separator = ", ", after = ")")
    }
    
    override def Match(tree: Match, selector: Tree, cases: List[Tree])(implicit ctx: PrintingContext) = {
      
      val leadingLayoutForFirstClause = cases match {
        case first :: _ if first.hasExistingCode =>
          surroundingLayoutFromParentsAndSiblings(cases.head)._1
        case _ => 
          NoLayout
      }
      
      val afterSelector = {
        if(leadingLayoutForFirstClause.asText.startsWith(" "))
          " match"
        else
          " match "
      }
      
      val selector_ = p(selector, after = afterSelector)
      
      val beforeClauses = {
        if(leadingLayoutForFirstClause.contains("{")) {
          NoRequisite
        } else {
          "{" ++ indentedNewline
        }
      }
      
      selector_ ++ ppi(cases, before = beforeClauses, separator = indentedNewline, after = newline ++ "}")
    }
  }
  
  trait MethodCallPrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def Select(tree: Select, qualifier: Tree, selector: Name)(implicit ctx: PrintingContext) = {
      (qualifier, selector) match {
        
        case (qualifier, name) if name.toString == "<init>" => 
          p(qualifier)
        
        case (qualifier, name) if name.toString == "package" => 
          p(qualifier)
        
        case (qualifier: This, selector) if qualifier.qual.toString == "immutable" =>
          Fragment(escapeScalaKeywordsForImport(tree.symbol.nameString))
          
        case (qualifier, selector) if (selector.toString == "unapply" || selector.toString == "unapplySeq") =>
          p(qualifier)
          
        case (_: Select | _: Ident | _: Block | _: Literal | _: Apply | _: This | _: Super, _) =>
          p(qualifier, after = ".") ++ Fragment(escapeScalaKeywordsForImport(tree.nameString))
          
        case _ =>
          p(qualifier, before = "(", after = ").") ++ Fragment(tree.nameString)   
      }    
    }
    
    override def TypeApply(tree: TypeApply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext) = {             
      fun match {
        
        case Select(Select(ths: This, selector), _) =>
          Fragment(selector.toString)
        
        case _ => 
          p(fun) ++ pp(args, before = "[", separator = ", ",  after = "]")
      }
    }
    
    override def Apply(tree: Apply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext) = {
      (fun, args) match {
           
        case (fun, args @ (Function(_, _: Match) :: _)) =>
          p(fun) ++ pp(args, before = " ")
          
        case (fun: Select, args @ ((arg1: Apply) :: _)) if fun.name.toString endsWith "_$eq" =>
          p(fun) ++ " = " ++ pp(args)
          
        case (fun: Select, arg :: Nil) if fun.name.toString endsWith "_$eq" =>
          p(fun) ++ " = " ++ p(arg)
          
        case (fun: Select, args) if fun.name.toString endsWith "_$eq" =>
          p(fun) ++ " = " ++ pp(args, before = "(", after = ")", separator = ", ")
          
        case (fun, (arg @ Ident(name)) :: Nil) if name.toString == "_" =>
          p(fun) ++ " " ++ p(arg)
          
        // the empty tree is an implicit parameter, so we mustn't print parenthesis
        case (fun, List(EmptyTree)) =>
          p(fun)
          
        case (Select(selector, _), arg :: Nil) =>
          val t = context("ignore") {
            p(selector).toLayout
          }
          
          if(t.withoutComments.endsWith(" "))
            p(fun) ++ " (" ++ p(arg) ++ ")"
          else
            p(fun) ++ "(" ++ p(arg) ++ ")"
          
        case _ =>
          p(fun) ++ balanceParens {
            EmptyFragment ++ "(" ++ pp(args, separator = ", ") ++ ")"
          }
      }
    }    
  }
  
  trait TypePrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def TypeTree(tree: TypeTree)(implicit ctx: PrintingContext) = {
      Fragment(typeToString(tree, tree.tpe))
    }
        
    override def Typed(tree: Typed, expr: Tree, tpt: Tree)(implicit ctx: PrintingContext): Fragment = {
      p(expr) ++ p(tpt)      
    }

    override def SingletonTypeTree(tree: SingletonTypeTree, ref: Tree)(implicit ctx: PrintingContext) = {
      p(ref) ++ Layout(".type")
    }

    override def CompoundTypeTree(tree: CompoundTypeTree, tpl: Template)(implicit ctx: PrintingContext) = {
      printTemplate(tpl, false)
    }
    
    override def AppliedTypeTree(tree: AppliedTypeTree, tpt: Tree, args: List[Tree])(implicit ctx: PrintingContext) = {
      p(tpt) ++ pp(args, before = "[", separator = ", ", after = "]")  
    }
    
    override def TypeBoundsTree(tree: TypeBoundsTree, lo: Tree, hi: Tree)(implicit ctx: PrintingContext) = {
      p(lo, before = ">: ", after = " ") ++ p(hi, before = "<: ")
    }
    
    override def ExistentialTypeTree(tree: ExistentialTypeTree, tpt: Tree, whereClauses: List[Tree])(implicit ctx: PrintingContext) = {
      whereClauses match {
        // [_]
        case (t: TypeDef) :: Nil if t.symbol.isSynthetic =>
          p(tpt) ++ p(t)
    
        case _ =>
          p(tpt) ++ pp(whereClauses, before = " forSome {", after = "}")
      }
    }
    
    override def TypeDef(tree: TypeDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], rhs: Tree)(implicit ctx: PrintingContext) = {
      
      tree match {
        
        case t @ global.TypeDef(ModifierTree(Nil), _, Nil, EmptyTree) if t.symbol.isSynthetic =>
          Fragment("[_]")
          
        case global.TypeDef(ModifierTree(mods), name, tparams, rhs) =>
          //mods.annotations map traverse
          val mods_ = mods map (m => m.nameString + " ") mkString ""
          val tparams_ = pp(tparams, before = "[", after = "]", separator = ", ")
          val rhs_ = rhs match {
            case rhs: TypeTree if rhs.original.isInstanceOf[TypeBoundsTree] => 
              p(rhs, before = " ")
            case rhs: TypeBoundsTree => 
              p(rhs, before = " ")
            case _ => 
              p(rhs, before = " = ")
          }
          Fragment(mods_ + tree.nameString) ++ tparams_ ++ rhs_ 
      }
    }
  }
  
  trait FunctionPrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def Function(tree: Function, vparams: List[ValDef], body: Tree)(implicit ctx: PrintingContext) = {

      val (args, bdy) = vparams match {
        
        case vparam :: Nil if !keepTree(vparam.tpt) =>
          val _body = p(body)
          
          if(_body.asText.startsWith("=>")) {
            (p(vparam) ++ Fragment(" "),  _body)
          } else {
            (p(vparam, before = "", after = " => "), _body)
          }
        case _ =>
          (pp(vparams, before = "(", separator = ", ", after = ") =>" ++ Requisite.Blank), p(body))
      }
      
      body match {
        // the body of the function contains more than one statement:
        case Block(_ :: Nil, _) if !bdy.asText.matches("(?ms)\\s*(=>)?\\s*\\{.*") => 
          Layout("{") ++ args ++ bdy ++ Layout(NL + ctx.ind.current + "}")
        case _ =>
          args ++ bdy
      }
    }
  }
  
  trait ImportPrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def Import(tree: Import, expr: Tree, selectors: List[ImportSelectorTree])(implicit ctx: PrintingContext) = {
      
      def renames(s: ImportSelector) = s.rename != null && s.name != s.rename
      
      val needsBraces = selectors.size > 1 || tree.selectors.exists(renames)
      
      def ss = (tree.selectors map { s =>
        escapeScalaKeywordsForImport(NameTransformer.decode(s.name.toString)) + {
          if(renames(s))
            " => " + escapeScalaKeywordsForImport(NameTransformer.decode(s.rename.toString))  
          else ""
        }
      } mkString ", ")
      
      expr match {
        case EmptyTree => EmptyFragment
        case _ if selectors.isEmpty => p(expr)
        case _ => Layout("import ") ++ p(expr) ++ Layout(".") ++ Fragment(if(needsBraces) "{" + ss + "}" else ss)
      }
    }
  }  
  
  trait PackagePrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def PackageDef(tree: PackageDef, pid: RefTree, stats: List[Tree])(implicit ctx: PrintingContext) = {
        
      if(pid.name.toString == "<empty>") {
        pp(stats, separator = newline)
      } else {
        pp(pid :: stats, before = "package ", separator = newline)
      }    
    }
  }

  trait TryThrowPrinters {
    this: TreePrinting with PrintingUtils =>

    override def Try(tree: Try, block: Tree, catches: List[Tree], finalizer: Tree)(implicit ctx: PrintingContext) = {
                          
      val _block = if(block.isInstanceOf[Block]) {
        p(block, before = "try ")
      } else {
        pi(block,before = "try {" ++ indentedNewline, after = newline ++ "}")
      }
      
      val _catches = ppi(catches, before = " catch {" ++ indentedNewline, separator = indentedNewline, after = newline ++ "}")
      
      val _finalizer = finalizer match {
        case EmptyTree => EmptyFragment
        case block: Block => p(block, before = " finally ")
        case _ => 
          pi(finalizer, before = " finally {" ++ indentedNewline, after = newline ++ "}")
      }
      
      _block ++ _catches ++ _finalizer
      //XXX create a "printBlock"
    }
    
    override def Throw(tree: Throw, expr: Tree)(implicit ctx: PrintingContext) = {
      Layout("throw ") ++ p(expr)
    }
  }  
  
  trait ClassModulePrinters {
    this: TreePrinting with PrintingUtils =>
  
    override def ClassDef(tree: ClassDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], impl: Template)(implicit ctx: PrintingContext) = {
      
      //mods.annotations map traverse
      val mods_ = mods map (m => m.nameString + " ") mkString ""
      
      val keyword = if(tree.mods.isTrait)
        "" // "trait" is a modifier
      else
        "class "
        
      val body = impl match {
        case TemplateExtractor(Nil :: Nil, _, _, _, _) if mods exists (_.nameString == "case") =>
          Layout("()") ++ p(impl)
        case _ => 
          p(impl)
      }
      
      Fragment(mods_ + keyword + name) ++ pp(tparams, before = "[", separator = ", ", after = "]") ++ body
    }
    
    override def ModuleDef(tree: ModuleDef, mods: List[ModifierTree], name: Name, impl: Template)(implicit ctx: PrintingContext) = {
      //        mods.annotations map traverse
            
      val mods_ = mods map (m => m.nameString + " ") mkString ""
      Fragment(mods_ + "object " + name) ++ p(impl)       
    }

    override def Template(tree: Template, parents: List[Tree], self: Tree, body: List[Tree])(implicit ctx: PrintingContext) = {
      printTemplate(tree, printExtends = true)
    }
  }
  
  trait IfPrinters {
    this: TreePrinting with PrintingUtils =>
  
    override def If(tree: If, cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: PrintingContext) = {
      
      val (thenLeadingLayout_, then_) = {
        pi(thenp) match {
          case f => (f.leading, f.dropLeadingLayout)
        }
      }

      val cond_ = balanceParens {
        p(cond, before = "if (", after = ")") ++ thenLeadingLayout_ ++ Requisite.Blank
      }
      
      val beforeElse = {
        if(thenLeadingLayout_.contains("{"))
          newline ++ "} else "
        else
          allowSurroundingWhitespace(" else ")
      }
      
      val else_ = p(elsep, before = beforeElse)
      
      cond_ ++ then_ ++ else_ 
    }
  }  
  
  trait ValDefDefPrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def ValDef(tree: ValDef, mods: List[ModifierTree], name: Name, tpt: Tree, rhs: Tree)(implicit ctx: PrintingContext) = {
         
      def needsKeyword(t: ValDef) =
        !t.mods.hasFlag(Flags.PARAM) &&
        !t.mods.hasFlag(Flags.PARAMACCESSOR) &&
        !t.mods.hasFlag(Flags.CASEACCESSOR) &&
        !t.mods.hasFlag(Flags.SYNTHETIC) &&
        !t.symbol.isSynthetic
      
      //mods.annotations map traverse
      val mods_ = {
        val existingMods = mods map (m => m.nameString + " ") mkString ""
        if(!tree.symbol.isMutable && needsKeyword(tree) && !existingMods.contains("val")) {
          existingMods + "val "
        } else {
          existingMods 
        }
      }
      
      val valName = if(tree.symbol.isThisSym && name.toString == "_") { // this: ... =>
        "this"
      } else name.toString.trim
              
      Fragment(mods_ + valName) ++ p(tpt, before = ": ") ++ p(rhs, before = " = ")    
    }
    
    override def DefDef(tree: DefDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)(implicit ctx: PrintingContext) = {
      //mods.annotations map traverse      

      val mods_ = {
        val existingMods = mods map (m => m.nameString + " ") mkString ""
        if(tree.mods.hasFlag(Flags.STABLE)&& !existingMods.contains("val")) {
          existingMods + "val "
        } else {
          existingMods 
        }
      }
      
      val tparams_ = {
        // Finalize this fragment so that the anywhere-requisite gets applied here
        // and does not match on ] that might come later (see testNewDefDefWithOriginalContent3
        // and testDefDefWithTypeParams).
          pp(tparams, before = "[", after = anywhere("]"), separator = ", ").toLayout 
      }
      
      // if there's existing layout, the type parameter's layout might already contain "()"
      val params_ = printParameterList(vparamss, tparams_.asText.matches(".*\\(.*\\).*"))
      
      val rhs = if(tree.rhs == EmptyTree && !tree.symbol.isDeferred) {
        Fragment(" {"+ NL + ctx.ind.current +"}")
      } else {
        p(tree.rhs, before = Requisite.allowSurroundingWhitespace("=", " = "))
      } 
      
      val resultType = {
        // The `:` has many places where it can hide, not just the adjoining layout,
        // so we have to check several places: if the parameter list is empty, it could
        // even be part of the tparams.
        val colon = new Requisite {
          def isRequired(l: Layout, r: Layout) = {
            !(l.contains(":") || r.contains(":") || {
              (tparams_.withoutComments + params_.withoutComments).matches(".*:\\s*")
            })
          }
          def getLayout = Layout(": ")
        }
        // Finalize the layout so the `:` won't be searched in the rhs.
        p(tpt, before = colon).toLayout
      }
      
      Fragment(mods_ + tree.nameString) ++ tparams_ ++ params_ ++ resultType ++ rhs         
    }
  }
   
  trait SuperPrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def SuperConstructorCall(tree: SuperConstructorCall, clazz: global.Tree, args: List[global.Tree])(implicit ctx: PrintingContext) = {
      p(clazz) ++ pp(args, before = "(", separator = ", ", after = ")")  
    }
    
    override def Super(tree: Super, qual: Tree, mix: Name)(implicit ctx: PrintingContext) = {
      
      val q = qual match {
        case This(qual: Name) if qual.toString == "" => EmptyFragment
        case This(qual: Name) => Fragment(qual.toString + ".")
        case _ => p(qual) // can this actually happen?
      }
      
      val m = if(mix.toString == "") "" else "["+ mix + "]"
      
      q ++ Fragment("super"+ m)      
    }
  }
   
  trait LiteralPrinters {
    this: TreePrinting with PrintingUtils =>
    override def Literal(tree: Literal, value: Constant)(implicit ctx: PrintingContext) = {
      
      if(value.tag == StringTag) {
        Fragment("\""+ value.stringValue +"\"")
      } else if (value.isNumeric) {
        val suffix = value.tag match {
          case FloatTag => "f"
          case LongTag => "l"
          case _ => ""
        }
        Fragment(value.stringValue + suffix)        
      } else { 
        Fragment(value.stringValue)
      }
    }
  }
   
  trait BlockPrinters {
    this: TreePrinting with PrintingUtils =>

    override def Block(tree: Block, stats: List[Tree])(implicit ctx: PrintingContext) = {
      
      def printWithEnclosing = ppi(stats, before = "{" ++ indentedNewline, separator = indentedNewline, after = newline ++ "}")
      
      // FIXME don't code when tired..
      
      if(stats.size > 1 && !stats.head.hasExistingCode && stats.tail.exists(_.hasExistingCode)) {
        
        val firstWithExistingCode = stats.find(_.hasExistingCode) get
        val printed = p(firstWithExistingCode)
        if(printed.leading.matches("(?ms).*\\{.*")) {
          
          val ExtractOpeningBrace = new scala.util.matching.Regex("(?ms)(.*\\{.*?)(\r?\n.*)")
          val ExtractOpeningBrace(leading, rest) = printed.leading.asText
          
          val printedStats = stats map { 
            case tree if tree == firstWithExistingCode =>
              Fragment(Layout(rest), printed.center, printed.trailing)
            case tree =>
              pi(tree)
          }
          
          Layout(leading) ++ indentedNewline ++ printedStats.foldLeft(EmptyFragment: Fragment)(_ ++ _)
          
        } else {
          printWithEnclosing
        }
      } else if (stats.head.hasExistingCode) {
        
        val printed = context("temp print") {
          p(stats.head)
        }
        
        if(printed.leading.matches("(?ms).*\\{.*")) {
          ppi(stats, separator = indentedNewline, after = newline ++ "}")
        } else
          printWithEnclosing
      } else 
        printWithEnclosing  
      
    } 
  }
  
  trait MiscPrinters {
    this: TreePrinting with PrintingUtils =>
    
    override def DocDef(tree: DocDef, comment: DocComment, definition: Tree)(implicit ctx: PrintingContext) = {
      ctx.ind.fixIndentation(comment.raw, "") ++ p(definition)
    }
    
    override def Assign(tree: Assign, lhs: Tree, rhs: Tree)(implicit ctx: PrintingContext) = {
      p(lhs) ++ " = " ++ p(rhs)
    }

    override def Return(tree: Return, expr: Tree)(implicit ctx: PrintingContext) = {
      Layout("return ") ++ p(expr)
    }
    
    override def New(tree: New, tpt: Tree)(implicit ctx: PrintingContext) = {
      Layout("new ") ++ p(tpt)
    }
    
    override def This(tree: This, qual: Name)(implicit ctx: PrintingContext) = {
      Fragment((if(qual.toString == "") "" else qual.toString +".") + "this")
    }
    
    override def Ident(tree: Ident, name: Name)(implicit ctx: PrintingContext) = {
      if (tree.symbol.isSynthetic && name.toString.contains("$"))
        Fragment("_")
      else  
        Fragment(name.toString)
    }
    
    override def ModifierTree(tree: ModifierTree, flag: Long)(implicit ctx: PrintingContext) = {
      Fragment(tree.nameString)
    }
    
    override def MultipleAssignment(tree: MultipleAssignment, extractor: Tree, values: List[ValDef], rhs: Tree)(implicit ctx: PrintingContext): Fragment = {
      extractor match {
        case EmptyTree =>
          Layout("val (") ++ pp(values, separator = ", ") ++ ") = " ++ p(rhs)
        case _ =>
          Layout("val ") ++ p(extractor) ++ " = " ++ p(rhs)
      }
    }
 
    
    override def SourceLayoutTree(tree: SourceLayoutTree)(implicit ctx: PrintingContext) = {
      tree.kind match {
        case SourceLayouts.Newline => Fragment(NL + NL + ctx.ind.current)
      }
    }
  }
}
