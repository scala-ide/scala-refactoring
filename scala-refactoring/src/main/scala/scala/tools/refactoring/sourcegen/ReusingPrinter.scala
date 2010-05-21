package scala.tools.refactoring
package sourcegen

trait ReusingPrinter extends AbstractPrinter {

  this: LayoutHelper with common.Tracing with common.PimpedTrees =>
  
  import global._
  
  def reuseExistingSource(traverse: (Tree, Indentation) => Option[Fragment], t: Tree, ind: Indentation): Fragment = context("Reuse "+ t.getClass.getSimpleName) { 
        
    val (leadingParent, leadingChild, trailingChild, trailingParent) = surroundingLayout(t)
    val allLayout = leadingParent.asText + leadingChild.asText + trailingChild.asText + trailingParent
    
    val thisIndentation = indentation(t)
    
    val (leading: Layout, center: Fragment) = (ind.setTo(thisIndentation) {
      printWithExistingLayout(traverse, t, ind, leadingChild, trailingChild)
    } match {
      case c if ind.text != thisIndentation && allLayout.contains("\n") => 
        Layout(fixIndentation(leadingParent.asText, t, thisIndentation, ind.text)) →
        Fragment(fixIndentation(c.asText, t, thisIndentation, ind.text))
      case c => leadingParent → c
    })
    
    Fragment(leading, center toLayout, trailingParent) \\ (r => trace("result "+ t.getClass.getSimpleName +": %s", r.toString))
  }
  
  private def fixIndentation(code: String, t: Tree, currentIndentation: String, desiredIndentation: String) = {
    trace("code is %s", code)
    trace("desired indentation is %s", desiredIndentation)
    trace("current indentation is %s", currentIndentation)
    code.replace("\n"+ currentIndentation, "\n"+ desiredIndentation)
  }

  private def printWithExistingLayout(f: (Tree, Indentation) => Option[Fragment], t: Tree, ind: Indentation, l: Layout, r: Layout): Fragment = {
        
    //it seems that default arguments here crash the incremental compiler, which is a real PITA, so we don't use them.
    object PrintOverloads {
      
      def print(tree: Tree): Fragment = 
        printSingleTree(t, tree, ind, f, indent = false, NoRequisite, NoRequisite)
      def printMany(ts: List[Tree]): Fragment = 
        printManyTrees(t, ts, ind, f, false, NoRequisite, NoRequisite, NoRequisite)
        
      def printMany(ts: List[Tree], separator: Requisite): Fragment =
        printManyTrees(t, ts, ind, f, false, separator, NoRequisite, NoRequisite)
      
      def print(tree: Tree, before: Requisite, after: Requisite): Fragment = 
        printSingleTree(t, tree, ind, f, false, before, after)
      def printMany(ts: List[Tree], separator: Requisite, before: Requisite, after: Requisite): Fragment = 
        printManyTrees(t, ts, ind, f, false, separator, before, after)
        
      def printIndented(tree: Tree, before: Requisite, after: Requisite): Fragment = 
        printSingleTree(t, tree, ind, f, true, before, after)
      def printManyIndented(ts: List[Tree], separator: Requisite): Fragment = 
        printManyTrees(t, ts, ind, f, true, separator, NoRequisite, NoRequisite)
        
    }
    
    import PrintOverloads._
    
    val newline = "\n"+ ind.text
     
    val originalTree = findOriginalTree(t) getOrElse {
      throw new Exception("original tree not found for: "+ t)
    }
    
    implicit def stringToRequisite(regex: String) = Requisite.allowSurroundingWhitespace(regex)
    
   (t, originalTree) match {
            
      case (t @ PackageDef(pid, stats), _) =>
        l ++ printManyIndented(pid :: stats, separator = Requisite.newline(ind.text)) ++ r
      
      case (t @ ClassDef(ModifierTree(mods), name, tparams, impl), orig) =>
        l ++ printMany(mods) ++ (if(t.symbol.isAnonymousClass) EmptyFragment else print(NameTree(name) setPos orig.namePosition)) ++ printMany(tparams) ++ print(impl) ++ r
        
      case (t @ ModuleDef(ModifierTree(mods), name, impl), orig) =>
        l ++ printMany(mods) ++ print(NameTree(name) setPos orig.namePosition) ++ print(impl) ++ r
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, Nil), _) =>
        l ++ printMany(params, separator = ",") ++ printMany(earlyBody) ++ printMany(parents) ++ print(self) ++ r
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, body), TemplateExtractor(_, _, _, _, origBody)) =>
        
        val preBody = l ++ printMany(params, separator = ",", before = NoRequisite, after = Requisite.anywhere(")")) ++ printMany(earlyBody) ++ printMany(parents) ++ print(self)
        
        if(origBody.size == 0) {
          //there might be an empty body present:
          val trailingLayout = if(r.matches("(?ms).*\\{.*\\}.*"))
            NoLayout
          else
            r
          
          val x = preBody ++ trailingLayout ++ SeparatedBy(" {"+newline) ++ printManyIndented(body, separator = Requisite.newline(ind.text)) ++ SeparatedBy(newline+"}")
          x
        } else {
          preBody ++ SeparatedBy(newline) ++ printManyIndented(body, separator = Requisite.newline(ind.text)) ++ r
        }
        
      case (t @ DefDef(ModifierTree(mods), newName, tparams, vparamss, tpt, rhs), orig) =>
        val nameTree = NameTree(t.nameString) setPos orig.namePosition
        l ++ printMany(mods ::: nameTree :: Nil, separator = Requisite.Blank) ++
          printMany(tparams) ++ vparamss.map(vparams => printMany(vparams, before = "\\(", separator = ",", after = Requisite.anywhere(")"))).foldLeft(EmptyFragment: Fragment)(_ ++ _) ++ print(tpt) ++ print(rhs) ++ r
        
      case (t @ ValDef(ModifierTree(mods), newName, tpt, rhs), orig) =>
        l ++ printMany(mods ::: (NameTree(newName) setPos orig.namePosition) :: Nil, separator = Requisite.Blank) ++ print(tpt) ++ print(rhs) ++ r

      case (BlockExtractor(stats), _) if stats.allOnSameLine => 
        l ++ printMany(stats) ++ r
        
      case (BlockExtractor(stats), _) => 
        val rest = printManyIndented(stats, separator = Requisite.newline(ind.text)) ++ r 
        if(l contains "{")
          l ++ SeparatedBy(newline) ++ rest
        else 
          l ++ rest
        
      case (t: TypeTree, _) if t.original == null && !t.pos.isTransparent => 
        l ++ Fragment(t.toString) ++ r
        
      case (t: TypeTree, _) => 
        l ++ print(t.original) ++ r
        
      case (t: AppliedTypeTree, _) => 
        l ++ print(t.tpt) ++ printMany(t.args) ++ r
        
      case (t @ TypeApply(Select(fun @ Select(ths: This, _), _), _), _) if ths.pos == NoPosition => 
        l ++ print(fun) ++ printMany(t.args) ++ r
        
      case (t: TypeApply, _) => 
        l ++ print(t.fun) ++ printMany(t.args) ++ r
        
      case (t @ TypeDef(ModifierTree(mods), name, tparams, rhs), orig) => 
        l ++ printMany(mods ::: (NameTree(name) setPos orig.namePosition) :: Nil, separator = Requisite.Blank) ++ printMany(tparams) ++ print(rhs)  ++ r
        
      case (t: Ident, _) => 
        l ++ Fragment(t.nameString)  ++ r
        
      // XXX List(..) has an invisible immutable.this qualifier
      case (t @ Select(qualifier: This, selector), _) if /*qualifier.qual.toString == "immutable" &&*/ qualifier.pos == NoPosition => 
        l ++ Fragment(t.symbol.nameString)  ++ r
        
      // skip <init> from constructor calls
      case (t @ Select(qualifier: New, selector), orig) if t.symbol.isConstructor =>
        l ++ print(qualifier)  ++ r
        
      case (t @ Select(qualifier, selector), _) if t.pos.sameRange(qualifier.pos) 
          && (selector.toString == "unapply" || selector.toString == "apply" || selector.toString == "unapplySeq") =>
        l ++ print(qualifier) ++ r
        
      case (t @ Select(qualifier, selector), orig) if selector.toString.startsWith("unary_")=>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        l ++ print(nameOrig) ++ print(qualifier) ++ r
        
      case (t @ Select(qualifier, selector), orig) =>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        l ++ print(qualifier) ++ print(nameOrig)  ++ r
      
      case (t: Literal, _) =>
        l ++ Fragment(t.toString)  ++ r
        
        
      case (t @ Apply(fun, args @ ((_: Bind) :: ( _: Bind) :: _)), _) =>
        l ++ print(fun) ++ printMany(args)  ++ r
        
      case (t @ Apply(fun: Select, arg :: Nil), _) if 
          (fun.qualifier != EmptyTree && keepTree(fun.qualifier)) /*has receiver*/
           || fun.name.toString.endsWith("$eq") /*assigns*/ =>
        if(r.contains(")")) {
          l ++ print(fun) ++ "\\(" ++ print(arg)  ++ r
        } else {
          l ++ print(fun) ++ print(arg)  ++ r
        }
        
      case (t @ Apply(fun @ TypeApply(_: Select, _), (arg @ Function(_, _: Match)) :: Nil), _) =>
        l ++ print(fun) ++ print(arg)  ++ r
        
      case (t @ Apply(fun @ TypeApply(_: Select, _), arg :: Nil), _) if !arg.isInstanceOf[Function] =>
        l ++ print(fun) ++ print(arg, before = "\\(", after = "\\)")  ++ r
        
      case (t @ Apply(fun, arg :: Nil), _) if !keepTree(fun) =>
        l ++ print(arg)  ++ r
        
      case (t @ Apply(EmptyTree, args), _) =>
        l ++ printMany(args, separator = ",", before = "\\(", after = "\\)")  ++ r 
        
      case (t @ Apply(fun, args), _) =>
        l ++ print(fun) ++ printMany(args, separator = ",", before = "\\(", after = "\\)")  ++ r
        
      case (t @ Import(expr, _), _) =>
        val ts = t.Selectors()
        if(ts.size > 1) {
          l ++ print(expr) ++ "\\{" ++ printMany(ts, separator = ", ") ++ "\\}" ++ r
        } else {
          l ++ print(expr) ++ printMany(ts, separator = ", ")  ++ r
        }
        
      case (t @ ImportSelectorTree(name, rename), _) =>
        l ++ print(name) ++ print(rename)  ++ r
        
      case (t: NameTree, _) =>
        if(t.pos.isTransparent) 
          l ++ EmptyFragment ++ r
        else 
          l ++ Fragment(t.nameString)  ++ r
        
      case (t @ SuperConstructorCall(clazz, args), _) =>
        l ++ print(clazz) ++ printMany(args, separator = ",", before = "\\(", after = "\\)") ++ r
        
      case (t @ SelfTypeTree(name, types, orig), _) =>
        l ++ print(name) ++ printMany(types) ++ r
        
      case (t: ModifierTree, _) =>
        l ++ Fragment(t.nameString) ++ r
        
      case (t @ Function(vparams, body), _) =>
        l ++ printMany(vparams) ++ print(body) ++ r

      case (t @ If(cond, thenp, elsep), orig: If) =>
        
        val _else = if(keepTree(orig.elsep) && orig.elsep.pos.isRange) {
          
          val layout = between(orig.thenp, orig.elsep)(orig.pos.source).asText
          
          if(elsep.isInstanceOf[Block]) {
            val l = Requisite.anywhere(layout.replaceAll("(?ms)else\\s*\n\\s*$", "else "))
            print(elsep, before = l, after = NoRequisite)
          } else {
            printIndented(elsep, before = Requisite.anywhere(layout), after = NoRequisite)
          }
        } else {
          val l = Requisite.newline(ind.text) ++ "else" ++ Requisite.newline(ind.text + ind.defaultIncrement)
          printIndented(elsep, before = l, after = NoRequisite)
        }
        
        l ++ print(cond, before = "\\(", after = "\\)") ++ printIndented(thenp, before = NoRequisite, after = NoRequisite) ++ _else ++ r
        
      case (This(qual), _) =>
        l ++ Fragment((if(qual.toString == "") "" else qual +".") + "this") ++ r
        
      case (Return(expr), _) =>
        l ++ print(expr) ++ r
        
      case (TypeBoundsTree(lo, hi), _) =>
        l ++ print(lo) ++ print(hi) ++ r
        
      case (t @ New(tpt), _) =>
        if(t.pos.start > t.pos.point)
          l ++ print(tpt) ++ r
        else
          l ++ Fragment("new") ++ print(tpt) ++ r
          
      case (Match(selector, cases), _) =>
        l ++ print(selector) ++ printMany(cases) ++ r
        
      case (CaseDef(pat, guard, body), _) =>
        l ++ print(pat) ++ print(guard) ++ print(body) ++ r
        
      case (Bind(name, body: Bind), orig) =>
        val nameOrig = NameTree(name) setPos orig.namePosition
        l ++ print(nameOrig) ++ print(body, before = "\\(", after = "\\)") ++ r
        
      case (Bind(name, body), orig) =>
        val nameOrig = NameTree(name) setPos orig.namePosition
        l ++ print(nameOrig) ++ print(body) ++ r
        
      case (Typed(expr, tpt), _) =>
        l ++ print(expr) ++ print(tpt) ++ r
        
      case (Alternative(trees), _) =>
        l ++ printMany(trees, separator = "|") ++ r
        
      case (UnApply(fun, args), _) =>
        l ++ print(fun) ++ printMany(args, separator = ",", before = "\\(", after = "\\)") ++ r
        
      case (Star(elem), _) =>
        l ++ print(elem) ++ r
        
      case (Super(qual, mix), _) =>
        val q = if(qual.toString == "") "" else qual +"."
        val m = if(mix.toString == "") "" else "["+ mix + "]"
        l ++ Fragment(q+ "super" +m) ++ r
   
      case (Try(block, catches, finalizer), _) =>
        l ++ print(block) ++ printMany(catches) ++ print(finalizer) ++ r
 
      case (t, _) => 
        println("printWithExistingLayout: "+ t.getClass.getSimpleName)
        l ++ Fragment("ø") ++ r
    }
  }
}