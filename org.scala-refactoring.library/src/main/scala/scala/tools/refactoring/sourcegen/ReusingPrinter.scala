/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

trait ReusingPrinter extends AbstractPrinter {

  this: LayoutHelper with common.Tracing with common.PimpedTrees with common.CompilerAccess with Indentations =>
  
  import global._
  
  def print(t: Tree, ind: Indentation, changeSet: ChangeSet): Fragment = context("Printing by reusing: "+ t.getClass.getSimpleName) { 
    
    val originalIndentation = indentation(t)

    val (leadingParent, leadingChild, trailingChild, trailingParent) = surroundingLayout(t)

    val printedFragment = if(changeSet hasChanged t) {
      val ctx = PrintingContext(ind.setTo(originalIndentation), changeSet, t)
      printWithExistingLayout(leadingChild, trailingChild)(ctx)
    } else {
      trace("Not in change set, keep original code.")
      Fragment(t.pos.source.content.slice(t.pos.start, t.pos.end).mkString)
    }
      
    val indentedFragment = {
      if(ind.needsToBeFixed(originalIndentation, leadingParent, leadingChild, trailingChild, trailingParent)) {
        val indentedLeadingLayout = ind.fixIndentation(leadingParent.asText, originalIndentation)
        val indentedCode = ind.fixIndentation(printedFragment.asText, originalIndentation)
        Fragment(indentedLeadingLayout, indentedCode, trailingParent)
      } else 
        Fragment(leadingParent, printedFragment toLayout, trailingParent)    
    } \\ (trace("Result "+ t.getClass.getSimpleName +": %s", _))
    
    indentedFragment
  }
  
  private def printWithExistingLayout(l: Layout, r: Layout)(implicit ctx: PrintingContext): Fragment = {
        
    import ctx._
    
    //it seems that default arguments here crash the incremental compiler, so we don't use them.
    object PrintOverloads {
      
      def p(tree: Tree): Fragment = 
        printSingleTree(tree, NoRequisite, NoRequisite)
      def p(ts: List[Tree]): Fragment = 
        printManyTrees(ts, NoRequisite, NoRequisite, NoRequisite)
        
      def p(ts: List[Tree], separator: Requisite): Fragment =
        printManyTrees(ts, separator, NoRequisite, NoRequisite)
      
      def p(tree: Tree, before: Requisite, after: Requisite): Fragment = 
        printSingleTree(tree, before, after)
      def p(ts: List[Tree], separator: Requisite, before: Requisite, after: Requisite): Fragment = 
        printManyTrees(ts, separator, before, after)
        
      def printIndented(tree: Tree, before: Requisite, after: Requisite): Fragment = 
        printIndentedSingleTree(tree, before, after)
      def printIndented(ts: List[Tree], separator: Requisite): Fragment = 
        printIndentedManyTrees(ts, separator, NoRequisite, NoRequisite)
    }
    
    import PrintOverloads._
    
    val originalTree = findOriginalTree(parent) getOrElse {
      trace("Original tree not found for %s, returning EmptyFragment.", parent)
      return EmptyFragment
    }
    
    implicit def stringToRequisite(regex: String) = Requisite.allowSurroundingWhitespace(regex)
    
    (parent, originalTree) match {
            
      case (t @ PackageDef(pid, stats), _) =>
        l ++ p(pid :: stats, separator = Requisite.newline(ind.current)) ++ r
      
      case (t @ ClassDef(ModifierTree(mods), name, tparams, impl), orig) =>
        l ++ p(mods) ++ (if(t.symbol.isAnonymousClass) EmptyFragment else p(NameTree(name) setPos orig.namePosition)) ++ p(tparams) ++ p(impl) ++ r
        
      case (t @ ModuleDef(ModifierTree(mods), name, impl), orig) =>
        l ++ p(mods) ++ p(NameTree(name) setPos orig.namePosition) ++ p(impl) ++ r
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, Nil), _) =>
        val _params = p(params, separator = ",", before = "\\(", after = "\\)")
        val _parents = p(parents)
        l ++ _params ++ p(earlyBody) ++ _parents ++ p(self) ++ r
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, body), o @ TemplateExtractor(_, _, _, _, origBody)) =>
        
        lazy val isExistingBodyAllOnOneLine = {
          val tplStartLine = o.pos.source.offsetToLine(o.pos.start)
          val tplEndLine = o.pos.source.offsetToLine(o.pos.end)
          tplStartLine == tplEndLine
        }
        
        val preBody = l ++ p(params, separator = ",", before = NoRequisite, after = Requisite.anywhere(")")) ++ p(earlyBody) ++ p(parents) ++ p(self)
        
        if(origBody.isEmpty && !body.isEmpty) {
          val alreadyHasBodyInTheCode = r.matches("(?ms).*\\{.*\\}.*") 
          val trailingLayout = if(alreadyHasBodyInTheCode) NoLayout else r
          
          val openingBrace = " \\{\n"+ ind.current
          val closingBrace = "\n"+ ind.current +"\\}"
          val bodyResult = printIndented(body, separator = Requisite.newline(ind.current))
          
          preBody ++ trailingLayout ++ openingBrace ++ bodyResult ++ closingBrace
        } else if (isExistingBodyAllOnOneLine) {
          preBody ++ printIndented(body, separator = Requisite.newline(ind.current)) ++ r
        } else {
          preBody ++ Requisite.newline(ind.current) ++ printIndented(body, separator = Requisite.newline(ind.current)) ++ r
        }
        
      case (t @ DefDef(ModifierTree(mods), newName, tparams, vparamss, tpt, rhs), orig: DefDef) =>
        val nameTree = NameTree(t.nameString) setPos orig.namePosition
        val modsAndName = p(mods ::: nameTree :: Nil, separator = Requisite.Blank)
        val parameters     = vparamss.map(vparams => p(vparams, before = "\\(", separator = ",", after = Requisite.anywhere(")"))).foldLeft(EmptyFragment: Fragment)(_ ++ _) 
        val typeParameters = p(tparams, before = "\\[", separator = ",", after = Requisite.anywhere("]"))
       
        l ++ modsAndName ++ typeParameters ++ parameters ++ p(tpt) ++ p(rhs) ++ r
          
      case (t @ ValDef(ModifierTree(mods), newName, tpt, rhs), orig) =>
        val nameTree = NameTree(newName) setPos orig.namePosition
        l ++ p(mods ::: nameTree :: Nil, separator = Requisite.Blank) ++ p(tpt) ++ p(rhs) ++ r

      case (block @ BlockExtractor(stats), _) if stats.size > 1 && stats.allOnSameLine => 
        l ++ p(stats) ++ r
        
      case (BlockExtractor(stats), _) =>
        val rest = printIndented(stats, separator = Requisite.newline(ind.current)) ++ r 
        if(l.contains("{") && !stats.head.hasExistingCode)
          l ++ Requisite.newline(ind.current, force = true) ++ rest
        else 
          l ++ rest
        
      case (t: TypeTree, _) if t.original == null && !t.pos.isTransparent => 
        t.tpe match {
          case ref @ RefinedType(_ :: parents, _) =>  
            l ++ Fragment(parents mkString " ") ++ r
          case t => 
            l ++ Fragment(t.toString) ++ r
        }
        
      case (t: TypeTree, _) => 
        l ++ p(t.original) ++ r
        
      case (t: AppliedTypeTree, _) => 
        l ++ p(t.tpt) ++ p(t.args) ++ r
        
      case (t @ TypeApply(Select(fun @ Select(ths: This, _), _), _), _) if ths.pos == NoPosition => 
        l ++ p(fun) ++ p(t.args) ++ r
        
      case (t: TypeApply, _) => 
        l ++ p(t.fun) ++ p(t.args) ++ r
        
      case (t @ TypeDef(ModifierTree(mods), name, tparams, rhs), orig) => 
        l ++ p(mods ::: (NameTree(t.nameString) setPos orig.namePosition) :: Nil, separator = Requisite.Blank) ++ p(tparams) ++ p(rhs)  ++ r
        
      case (t: Ident, _) => 
        l ++ Fragment(t.nameString) ++ r
        
      case (t @ Select(qualifier, name), _) if name.toString == "<init>" => 
        l ++ p(qualifier) ++ r
        
      case (t @ Select(qualifier: This, selector), _) if qualifier.pos == NoPosition => 
        l ++ Fragment(t.symbol.nameString) ++ r
        
      // skip <init> from constructor calls
      case (t @ Select(qualifier: New, selector), orig) if t.symbol.isConstructor =>
        l ++ p(qualifier) ++ r
        
      case (t @ Select(qualifier, selector), _) if t.pos.sameRange(qualifier.pos) 
          && (selector.toString == "unapply" || selector.toString == "apply" || selector.toString == "unapplySeq") =>
        l ++ p(qualifier) ++ r
        
      case (t @ Select(qualifier: Apply, selector), orig) if selector.toString.startsWith("unary_") =>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        val printedQualifier = p(qualifier)
        if(printedQualifier.asText.contains(" ")) //XXX better check to see if we need to print parens
          l ++ p(nameOrig) ++ "\\(" ++ printedQualifier ++ "\\)" ++ r          
        else
          l ++ p(nameOrig) ++ printedQualifier ++ r
        
      case (t @ Select(qualifier, selector), orig) if selector.toString.startsWith("unary_") =>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        l ++ p(nameOrig) ++ p(qualifier) ++ r
        
       case (t @ Select(qualifier @ Apply(s @ Select(qual, name), Nil), selector), orig) =>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        val sName = NameTree(s.nameString) setPos s.namePosition
        val l = between(qual, sName)(s.pos.source)
        
        val _qualifier = p(qualifier)
        
        if(!_qualifier.asText.matches("^\\s*\\(.*\\)\\s*") && l.contains(" ")) {
          l ++ "\\(" ++ _qualifier ++ "\\)" ++ " " ++ p(nameOrig) ++ r
        } else {
          l ++ _qualifier ++ p(nameOrig) ++ r
        }
        
      case (t @ Select(qualifier, selector), orig: Select) =>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        val _q = p(qualifier)
        val _n = p(nameOrig)

        def hasNoSeparator = {
          val between = (_q.trailing ++ _n.leading).asText
          !between.contains(" ") && !between.contains(".")
        }
        
        def startsWithChar = _q.asText.matches(".*[a-zA-Z0-9]$")
        def endsWithChar   = _n.asText.matches("^[a-zA-Z0-9].*")
        
        def qualifierHasNoDot = qualifier match {
          case Apply(s @ Select(qual, name), _) if qual.pos.isRange => 
            val nme = NameTree(name).setPos(s.namePosition)
            val b = between(qual, nme)(qual.pos.source)
            !b.contains(".")
          case _ => false
        }
        
        if(startsWithChar && endsWithChar && hasNoSeparator) {
          l ++ _q ++ " " ++ _n ++ r
        } else if (qualifierHasNoDot && _n.leading.contains(".")) {
          l ++ "\\(" ++ _q ++ "\\)" ++ _n ++ r
        } else {
          l ++ _q ++ _n ++ r
        }
      
      case (t: Literal, _) if t.value.tag == StringTag =>
        val escaped = t.value.stringValue.replace("""\""", """\\""")
        l ++ Fragment("\""+ escaped +"\"")  ++ r
        
      case (t: Literal, _) =>
        l ++ Fragment(t.value.stringValue) ++ r
        
      // handle e.g. a += 1 which is a = (a + 1)
      case (t @ Apply(fun: Select, args @ ((arg1: Apply) :: _)), _) if t.pos.sameRange(arg1.pos) && arg1.pos.isTransparent =>
        l ++ p(fun) ++ between(fun, arg1.args.head)(t.pos.source) ++ p(arg1.args) ++ r
        
      // x :: xs in pattern match:
      case (t @ Apply(EmptyTree, args @ ((_: Bind) :: ( _: Bind) :: _)), _) if t.tpe.toString.contains("::") =>
        l ++ p(args) ++ r
        
      case (t @ Apply(fun, args @ ((_: Bind) :: ( _: Bind) :: _)), _) =>
        l ++ p(fun) ++ p(args, before = if(l contains "(") NoRequisite else "\\(", separator = ",", after = "\\)") ++ r
        
      case (t @ Apply(fun: Select, arg :: Nil), _) if 
          (fun.qualifier != EmptyTree && keepTree(fun.qualifier)) /*has receiver*/
           || fun.name.toString.endsWith("$eq") /*assigns*/ =>
        if(r.contains(")")) {
          l ++ p(fun) ++ "\\(" ++ p(arg) ++ r
        } else {
          l ++ p(fun) ++ p(arg) ++ r
        }
        
      case (t @ Apply(fun @ TypeApply(_: Select, _), (arg @ Function(_, _: Match)) :: Nil), _) =>
        l ++ p(fun) ++ p(arg) ++ r
        
      case (t @ Apply(fun @ TypeApply(receiver: Select, _), arg :: Nil), _) if !arg.isInstanceOf[Function] =>
        if(keepTree(receiver.qualifier) && !l.contains("(") && !r.contains(")"))  {
          l ++ p(fun) ++ p(arg) ++ r
        } else {
          l ++ p(fun) ++ p(arg, before = Requisite.anywhere("("), after = Requisite.anywhere(")")) ++ r
        }
        
      case (t @ Apply(fun, arg :: Nil), _) if !keepTree(fun) =>
        l ++ p(arg) ++ r
        
      case (t @ Apply(EmptyTree, args), _) =>
        l ++ p(args, separator = ",", before = "\\(", after = "\\)")  ++ r 
        
      case (t @ Apply(fun, args), _) =>
        l ++ p(fun) ++ p(args, separator = ",", before = "\\(", after = Requisite.anywhere(")"))  ++ r
        
      case (t @ Import(expr, _), _) if t.Selectors().size > 1 =>
        l ++ "import " ++ p(expr) ++ "\\{" ++ p(t.Selectors(), separator = ", ") ++ "\\}" ++ r
        
      case (t @ Import(expr, _), _) =>
        l ++ "import " ++ p(expr) ++ p(t.Selectors(), separator = ", ") ++ r
        
      case (t @ ImportSelectorTree(name, rename), _) =>
        l ++ p(name) ++ p(rename) ++ r
        
      case (MultipleAssignment(values, rhs), _) =>
        l ++ p(values, separator = ",") ++ "\\)" ++ p(rhs) ++ r  
        
      case (t: NameTree, _) if t.pos.isTransparent =>
        l ++ EmptyFragment ++ r
          
      case (t: NameTree, _) =>
        l ++ Fragment(t.nameString) ++ r
        
      case (t @ SuperConstructorCall(clazz, args), _) =>
        val after = if(r.contains(")")) NoRequisite else Requisite.allowSurroundingWhitespace("\\)")
        l ++ p(clazz) ++ p(args, separator = ",", before = "\\(", after = after) ++ r
        
      case (t @ SelfTypeTree(name, types, orig), _) =>
        l ++ p(name) ++ p(types) ++ r
        
      case (t: ModifierTree, _) =>
        l ++ Fragment(t.nameString) ++ r
        
      case (t @ Function(vparams, b @ BlockExtractor(body)), _) if !b.hasExistingCode =>
        l ++ p(vparams) ++ ind.current ++ printIndented(body, separator = Requisite.newline(ind.current)) ++ r
        
      case (t @ Function(vparams, body), _) =>
        l ++ p(vparams) ++ p(body) ++ r

      case (t @ If(cond, thenp, elsep), orig: If) =>
                
        val _else = {
          
          /*
           * Printing the else branch is tricky because of how {} are handled in the AST,
           * but only if the else branch already existed:
           */
          val elseBranchAlreadyExisted = keepTree(orig.elsep) && orig.elsep.pos.isRange
          
          if(elseBranchAlreadyExisted) {
            
            val layout = between(orig.thenp, orig.elsep)(orig.pos.source).asText
            val l = Requisite.anywhere(layout.replaceAll("(?ms)else\\s*\n\\s*$", "else "))
            
            val curlyBracesAlreadyExist = layout.contains("{")
            val originalElseHasNoBlock = !orig.elsep.isInstanceOf[Block]
            
            elsep match {
              
              /*
               * The existing else branch was enclosed by {} but contained only a single
               * statement.
               * */
              case BlockExtractor(body) if originalElseHasNoBlock && curlyBracesAlreadyExist =>
                p(body, before = l, separator = Requisite.newline(ind.current + ind.defaultIncrement), after = NoRequisite)
              
              /*
               * If there was no block before and also no curly braces, we have to write
               * them now (indirectly through the Block), but we don't want to add any
               * indentation.
               * */
              case elsep: Block =>
                print(elsep, ind, changeSet) ifNotEmpty (_ ++ (NoRequisite, l))

              /* If it's a single statemens, we print it indented: */
              case _ => 
                printIndented(elsep, before = Requisite.anywhere(layout), after = NoRequisite)
            }

          } else {
            val l = Requisite.newline(ind.current) ++ "else" ++ Requisite.newline(ind.current + ind.defaultIncrement)
            printIndented(elsep, before = l, after = NoRequisite)
          }
        }
        
        val _cond = p(cond, before = "\\(", after = Requisite.anywhere(")"))
        
        val _then = thenp match {
          case block: Block =>
            p(block)
          case _ if keepTree(orig.thenp) && orig.thenp.pos.isRange =>
            val layout = between(orig.cond, orig.thenp)(orig.pos.source).asText
            val printedThen = printIndented(thenp, before = NoRequisite, after = NoRequisite)
            
            if(layout.contains("{") && !printedThen.asText.matches("(?ms)^\\s*\\{.*")) {
              val (left, right) = layout.splitAt(layout.indexOf(")") + 1)
              println(left)
              printIndented(thenp, before = Requisite.anywhere(right), after = NoRequisite)
            } else {
              printIndented(thenp, before = NoRequisite, after = NoRequisite)
            }
            
          case _ => 
            printIndented(thenp, before = NoRequisite, after = NoRequisite)
        }
        
        l ++ _cond ++ _then ++ _else ++ r
        
      case (This(qual), _) =>
        l ++ Fragment((if(qual.toString == "") "" else qual +".") + "this") ++ r
        
      case (Return(expr), _) =>
        l ++ p(expr) ++ r
        
      case (TypeBoundsTree(lo, hi), _) =>
        l ++ p(lo) ++ p(hi) ++ r
        
      case (t @ New(tpt), _) if t.pos.start > t.pos.point =>
        l ++ p(tpt) ++ r
          
      case (t @ New(tpt), _) =>
        Fragment("new") ++ l ++ p(tpt) ++ r
          
      case (Match(selector, cases), _) if keepTree(selector) =>
        l ++ p(selector) ++ " match" ++ p(cases) ++ r
          
      case (Match(selector, cases), _) =>
        l ++ p(cases) ++ r
        
      case (CaseDef(pat, guard, b @ BlockExtractor(body)), _) if !b.hasExistingCode =>
        l ++ p(pat) ++ p(guard) ++ "=>" ++ "\n" ++ Fragment("") ++ ind.current ++ printIndented(body, separator = Requisite.newline(ind.current)) ++ r
        
      case (CaseDef(pat, guard, body), _) =>
        l ++ p(pat) ++ p(guard) ++ p(body) ++ r
        
      case (Bind(name, body: Bind), orig) =>
        val nameOrig = NameTree(name) setPos orig.namePosition
        l ++ p(nameOrig) ++ p(body, before = "\\(", after = "\\)") ++ r
        
      case (Bind(name, body), orig) =>
        val nameOrig = NameTree(name) setPos orig.namePosition
        l ++ p(nameOrig) ++ p(body) ++ r
        
      case (Typed(expr, tpt), _) =>
        l ++ p(expr) ++ p(tpt) ++ r
        
      case (Alternative(trees), _) =>
        l ++ p(trees, separator = "|") ++ r
        
      case (UnApply(fun, args), _) =>
        l ++ p(fun) ++ p(args, separator = ",", before = "\\(", after = "\\)") ++ r
        
      case (Star(elem), _) =>
        l ++ p(elem) ++ r
        
      case (Assign(lhs, rhs), _) =>
        l ++ p(lhs, after = "=", before = NoRequisite) ++ p(rhs) ++ r
        
      case (Super(qual, mix), _) =>
        val q = if(qual.toString == "") "" else qual +"."
        val m = if(mix.toString == "") "" else "["+ mix + "]"
        l ++ Fragment(q+ "super" +m) ++ r
        
      case (Try(b @ BlockExtractor(block), catches, finalizer), _) if !b.hasExistingCode =>
        l ++ ind.current ++ printIndented(block, separator = Requisite.newline(ind.current)) ++ p(catches) ++ p(finalizer) ++ r
   
      case (Try(block, catches, finalizer), _) =>
        l ++ p(block) ++ p(catches) ++ p(finalizer) ++ r
        
      case (Throw(expr), _) =>
        l ++ p(expr) ++ r
        
      case (label @ LabelDef(_, _, Block(stats, If(cond, _, _))), orig) =>
        val labelName = (NameTree(label.nameString) setPos orig.namePosition)
        l ++ p(stats) ++ p(labelName) ++ Layout("(") ++ p(cond) ++ r
        
      case (label @ LabelDef(_, _, If(cond, Block((body: Block) :: Nil, _), _)), orig) =>
        val labelName = (NameTree(label.nameString) setPos orig.namePosition)
        l ++ p(labelName) ++ Layout("(") ++ p(cond) ++ Layout(")") ++ p(body) ++ r
        
      case (label @ LabelDef(_, _, If(cond, then, _)), orig) =>
        val labelName = (NameTree(label.nameString) setPos orig.namePosition)
        l ++ p(labelName) ++ Layout("(") ++ p(cond) ++ Layout(")") ++ printIndented(then, before = NoRequisite, after = NoRequisite) ++ r
 
      case (ExistentialTypeTree(tpt, whereClauses), _) =>
        l ++ p(tpt) ++ p(whereClauses) ++r
        
      case (t @ SelectFromTypeTree(qualifier, _), orig) =>
        l ++ p(qualifier) ++ p(NameTree(t.nameString) setPos orig.namePosition) ++ r
        
      case (SingletonTypeTree(ref), _) =>
        l ++ p(ref) ++ r
        
      case (CompoundTypeTree(templ), _) =>
        l ++ p(templ) ++ r
       
      case (t, _) => 
        println("printWithExistingLayout: "+ t.getClass.getSimpleName)
        l ++ Fragment("ø") ++ r
    }
  }
}