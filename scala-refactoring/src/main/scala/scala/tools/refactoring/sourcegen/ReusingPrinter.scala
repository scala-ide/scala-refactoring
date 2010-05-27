package scala.tools.refactoring
package sourcegen

trait ReusingPrinter extends AbstractPrinter {

  this: LayoutHelper with common.Tracing with common.PimpedTrees =>
  
  import global._
  
  def print(t: Tree, ind: Indentation): Fragment = context("Reuse "+ t.getClass.getSimpleName) { 
        
    val (leadingParent, leadingChild, trailingChild, trailingParent) = surroundingLayout(t)
    val allLayout = leadingParent.asText + leadingChild.asText + trailingChild.asText + trailingParent
    
    val thisIndentation = indentation(t)
    
    val (leading: Layout, center: Fragment) = printWithExistingLayout(t, ind.setTo(thisIndentation), leadingChild, trailingChild) match {
      case c if ind.current != thisIndentation && allLayout.contains("\n") => 
        Layout(fixIndentation(leadingParent.asText, t, thisIndentation, ind.current)) →
        Fragment(fixIndentation(c.asText, t, thisIndentation, ind.current))
      case c => leadingParent → c
    }
    
    Fragment(leading, center toLayout, trailingParent) \\ (r => trace("result "+ t.getClass.getSimpleName +": %s", r.toString))
  }
  
  private def fixIndentation(code: String, t: Tree, currentIndentation: String, desiredIndentation: String) = {
    trace("code is %s", code)
    trace("desired indentation is %s", desiredIndentation)
    trace("current indentation is %s", currentIndentation)
    code.replace("\n"+ currentIndentation, "\n"+ desiredIndentation)
  }

  private def printWithExistingLayout(t: Tree, ind: Indentation, l: Layout, r: Layout): Fragment = {
        
    //it seems that default arguments here crash the incremental compiler, which is a real PITA, so we don't use them.
    object PrintOverloads {
      
      def p(tree: Tree): Fragment = 
        printSingleTree(t, tree, ind, indent = false, NoRequisite, NoRequisite)
      def p(ts: List[Tree]): Fragment = 
        printManyTrees(t, ts, ind, false, NoRequisite, NoRequisite, NoRequisite)
        
      def p(ts: List[Tree], separator: Requisite): Fragment =
        printManyTrees(t, ts, ind, false, separator, NoRequisite, NoRequisite)
      
      def p(tree: Tree, before: Requisite, after: Requisite): Fragment = 
        printSingleTree(t, tree, ind, false, before, after)
      def p(ts: List[Tree], separator: Requisite, before: Requisite, after: Requisite): Fragment = 
        printManyTrees(t, ts, ind, false, separator, before, after)
        
      def printIndented(tree: Tree, before: Requisite, after: Requisite): Fragment = 
        printSingleTree(t, tree, ind, true, before, after)
      def printIndented(ts: List[Tree], separator: Requisite): Fragment = 
        printManyTrees(t, ts, ind, true, separator, NoRequisite, NoRequisite)
    }
    
    import PrintOverloads._
    
    val newline = "\n"+ ind.current
     
    val originalTree = findOriginalTree(t) getOrElse {
      throw new Exception("original tree not found for: "+ t)
    }
    
    implicit def stringToRequisite(regex: String) = Requisite.allowSurroundingWhitespace(regex)
    
   (t, originalTree) match {
            
      case (t @ PackageDef(pid, stats), _) =>
        l ++ printIndented(pid :: stats, separator = Requisite.newline(ind.current)) ++ r
      
      case (t @ ClassDef(ModifierTree(mods), name, tparams, impl), orig) =>
        l ++ p(mods) ++ (if(t.symbol.isAnonymousClass) EmptyFragment else p(NameTree(name) setPos orig.namePosition)) ++ p(tparams) ++ p(impl) ++ r
        
      case (t @ ModuleDef(ModifierTree(mods), name, impl), orig) =>
        l ++ p(mods) ++ p(NameTree(name) setPos orig.namePosition) ++ p(impl) ++ r
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, Nil), _) =>
        l ++ p(params, separator = ",") ++ p(earlyBody) ++ p(parents) ++ p(self) ++ r
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, body), TemplateExtractor(_, _, _, _, origBody)) =>
        
        val preBody = l ++ p(params, separator = ",", before = NoRequisite, after = Requisite.anywhere(")")) ++ p(earlyBody) ++ p(parents) ++ p(self)
        
        if(origBody.size == 0) {
          //there might be an empty body present:
          val trailingLayout = if(r.matches("(?ms).*\\{.*\\}.*"))
            NoLayout
          else
            r
          
          val x = preBody ++ trailingLayout ++ Requisite.allowSurroundingWhitespace(" \\{"+newline) ++ printIndented(body, separator = Requisite.newline(ind.current)) ++ Requisite.allowSurroundingWhitespace(newline +"\\}")
          x
        } else {
          preBody ++ Requisite.newline(ind.current) ++ printIndented(body, separator = Requisite.newline(ind.current)) ++ r
        }
        
      case (t @ DefDef(ModifierTree(mods), newName, tparams, vparamss, tpt, rhs), orig) =>
        val nameTree = NameTree(t.nameString) setPos orig.namePosition
        l ++ p(mods ::: nameTree :: Nil, separator = Requisite.Blank) ++
          p(tparams) ++ vparamss.map(vparams => p(vparams, before = "\\(", separator = ",", after = Requisite.anywhere(")"))).foldLeft(EmptyFragment: Fragment)(_ ++ _) ++ p(tpt) ++ p(rhs) ++ r
        
      case (t @ ValDef(ModifierTree(mods), newName, tpt, rhs), orig) =>
        l ++ p(mods ::: (NameTree(newName) setPos orig.namePosition) :: Nil, separator = Requisite.Blank) ++ p(tpt) ++ p(rhs) ++ r

      case (BlockExtractor(stats), _) if stats.allOnSameLine => 
        l ++ p(stats) ++ r
        
      case (BlockExtractor(stats), _) => 
        val rest = printIndented(stats, separator = Requisite.newline(ind.current)) ++ r 
        if(l contains "{")
          l ++ Requisite.newline(ind.current) ++ rest
        else 
          l ++ rest
        
      case (t: TypeTree, _) if t.original == null && !t.pos.isTransparent => 
        l ++ Fragment(t.toString) ++ r
        
      case (t: TypeTree, _) => 
        l ++ p(t.original) ++ r
        
      case (t: AppliedTypeTree, _) => 
        l ++ p(t.tpt) ++ p(t.args) ++ r
        
      case (t @ TypeApply(Select(fun @ Select(ths: This, _), _), _), _) if ths.pos == NoPosition => 
        l ++ p(fun) ++ p(t.args) ++ r
        
      case (t: TypeApply, _) => 
        l ++ p(t.fun) ++ p(t.args) ++ r
        
      case (t @ TypeDef(ModifierTree(mods), name, tparams, rhs), orig) => 
        l ++ p(mods ::: (NameTree(name) setPos orig.namePosition) :: Nil, separator = Requisite.Blank) ++ p(tparams) ++ p(rhs)  ++ r
        
      case (t: Ident, _) => 
        l ++ Fragment(t.nameString)  ++ r
        
      // XXX List(..) has an invisible immutable.this qualifier
      case (t @ Select(qualifier: This, selector), _) if /*qualifier.qual.toString == "immutable" &&*/ qualifier.pos == NoPosition => 
        l ++ Fragment(t.symbol.nameString)  ++ r
        
      // skip <init> from constructor calls
      case (t @ Select(qualifier: New, selector), orig) if t.symbol.isConstructor =>
        l ++ p(qualifier)  ++ r
        
      case (t @ Select(qualifier, selector), _) if t.pos.sameRange(qualifier.pos) 
          && (selector.toString == "unapply" || selector.toString == "apply" || selector.toString == "unapplySeq") =>
        l ++ p(qualifier) ++ r
        
      case (t @ Select(qualifier, selector), orig) if selector.toString.startsWith("unary_")=>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        l ++ p(nameOrig) ++ p(qualifier) ++ r
        
      case (t @ Select(qualifier, selector), orig) =>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        l ++ p(qualifier) ++ p(nameOrig)  ++ r
      
      case (t: Literal, _) =>
        l ++ Fragment(t.toString)  ++ r
        
        
      case (t @ Apply(fun, args @ ((_: Bind) :: ( _: Bind) :: _)), _) =>
        l ++ p(fun) ++ p(args)  ++ r
        
      case (t @ Apply(fun: Select, arg :: Nil), _) if 
          (fun.qualifier != EmptyTree && keepTree(fun.qualifier)) /*has receiver*/
           || fun.name.toString.endsWith("$eq") /*assigns*/ =>
        if(r.contains(")")) {
          l ++ p(fun) ++ "\\(" ++ p(arg)  ++ r
        } else {
          l ++ p(fun) ++ p(arg)  ++ r
        }
        
      case (t @ Apply(fun @ TypeApply(_: Select, _), (arg @ Function(_, _: Match)) :: Nil), _) =>
        l ++ p(fun) ++ p(arg)  ++ r
        
      case (t @ Apply(fun @ TypeApply(_: Select, _), arg :: Nil), _) if !arg.isInstanceOf[Function] =>
        l ++ p(fun) ++ p(arg, before = "\\(", after = "\\)")  ++ r
        
      case (t @ Apply(fun, arg :: Nil), _) if !keepTree(fun) =>
        l ++ p(arg)  ++ r
        
      case (t @ Apply(EmptyTree, args), _) =>
        l ++ p(args, separator = ",", before = "\\(", after = "\\)")  ++ r 
        
      case (t @ Apply(fun, args), _) =>
        l ++ p(fun) ++ p(args, separator = ",", before = "\\(", after = "\\)")  ++ r
        
      case (t @ Import(expr, _), _) =>
        val ts = t.Selectors()
        if(ts.size > 1) {
          l ++ p(expr) ++ "\\{" ++ p(ts, separator = ", ") ++ "\\}" ++ r
        } else {
          l ++ p(expr) ++ p(ts, separator = ", ")  ++ r
        }
        
      case (t @ ImportSelectorTree(name, rename), _) =>
        l ++ p(name) ++ p(rename)  ++ r
        
      case (t: NameTree, _) =>
        if(t.pos.isTransparent) 
          l ++ EmptyFragment ++ r
        else 
          l ++ Fragment(t.nameString)  ++ r
        
      case (t @ SuperConstructorCall(clazz, args), _) =>
        l ++ p(clazz) ++ p(args, separator = ",", before = "\\(", after = "\\)") ++ r
        
      case (t @ SelfTypeTree(name, types, orig), _) =>
        l ++ p(name) ++ p(types) ++ r
        
      case (t: ModifierTree, _) =>
        l ++ Fragment(t.nameString) ++ r
        
      case (t @ Function(vparams, body), _) =>
        l ++ p(vparams) ++ p(body) ++ r

      case (t @ If(cond, thenp, elsep), orig: If) =>
        
        val _else = if(keepTree(orig.elsep) && orig.elsep.pos.isRange) {
          
          val layout = between(orig.thenp, orig.elsep)(orig.pos.source).asText
          
          if(elsep.isInstanceOf[Block]) {
            val l = Requisite.anywhere(layout.replaceAll("(?ms)else\\s*\n\\s*$", "else "))
            p(elsep, before = l, after = NoRequisite)
          } else {
            printIndented(elsep, before = Requisite.anywhere(layout), after = NoRequisite)
          }
        } else {
          val l = Requisite.newline(ind.current) ++ "else" ++ Requisite.newline(ind.current + ind.defaultIncrement)
          printIndented(elsep, before = l, after = NoRequisite)
        }
        
        l ++ p(cond, before = "\\(", after = "\\)") ++ printIndented(thenp, before = NoRequisite, after = NoRequisite) ++ _else ++ r
        
      case (This(qual), _) =>
        l ++ Fragment((if(qual.toString == "") "" else qual +".") + "this") ++ r
        
      case (Return(expr), _) =>
        l ++ p(expr) ++ r
        
      case (TypeBoundsTree(lo, hi), _) =>
        l ++ p(lo) ++ p(hi) ++ r
        
      case (t @ New(tpt), _) =>
        if(t.pos.start > t.pos.point)
          l ++ p(tpt) ++ r
        else
          l ++ Fragment("new") ++ p(tpt) ++ r
          
      case (Match(selector, cases), _) =>
        l ++ p(selector) ++ p(cases) ++ r
        
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
        
      case (Super(qual, mix), _) =>
        val q = if(qual.toString == "") "" else qual +"."
        val m = if(mix.toString == "") "" else "["+ mix + "]"
        l ++ Fragment(q+ "super" +m) ++ r
   
      case (Try(block, catches, finalizer), _) =>
        l ++ p(block) ++ p(catches) ++ p(finalizer) ++ r
 
      case (t, _) => 
        println("printWithExistingLayout: "+ t.getClass.getSimpleName)
        l ++ Fragment("ø") ++ r
    }
  }
}