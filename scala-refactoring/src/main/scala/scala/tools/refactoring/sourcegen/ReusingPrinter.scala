package scala.tools.refactoring
package sourcegen

trait ReusingPrinter extends regeneration.SourceCodeHelpers {

  self: LayoutHelper with common.Tracing with common.PimpedTrees with PrettyPrinter =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def reuseExistingSource(traverse: (Tree, Indentation) => Option[Fragment], t: Tree, ind: Indentation): Fragment = context("Reuse "+ t.getClass.getSimpleName) { 
    
    trace("Base indentation is %s", ind.text)
    
    val (leadingParent, leadingChild, trailingChild, trailingParent) = surroundingLayout(t)
    val allLayout = leadingParent.asText + leadingChild.asText + trailingChild.asText + trailingParent
    
    val thisIndentation = indentation(t)
    
    val center: Fragment = ind.setTo(thisIndentation) {
      printWithExistingLayout(traverse, t, ind, leadingChild, trailingChild)
    } match {
      case c if allLayout contains "\n" => Fragment(fixIndentation(c.asText, t, thisIndentation, ind.text)) 
      case c => c
    }
    
    Fragment(leadingParent, center toLayout, trailingParent) \\ (r => trace("result: %s", r.toString))
  }
  
  private def fixIndentation(code: String, t: Tree, currentIndentation: String, parentIndentation: String) = {
    
    val originalParentIndentation = findOriginalTree(t) flatMap (_.originalParent) map indentation getOrElse parentIndentation // on the top level

    lazy val indentationDifference = if(originalParentIndentation.length < parentIndentation.length) {
        parentIndentation substring originalParentIndentation.length
      } else {
        originalParentIndentation substring parentIndentation.length
      }
    
    if(originalParentIndentation == parentIndentation) {
      code
    } else {
              
      trace("indentation currently is  %s", currentIndentation)
      trace("orig parent indentatio is %s", originalParentIndentation)
      trace("new parent indentatio is  %s", parentIndentation)
      
      indentationDifference + code.replace(currentIndentation, currentIndentation + indentationDifference)
    }
  }

  private def printWithExistingLayout(f: (Tree, Indentation) => Option[Fragment], t: Tree, ind: Indentation, l: Layout, r: Layout): Fragment = {
    
    implicit val currentFile = t.pos.source
    
    def newline = "\n"+ ind.text
    
    def handle(t: Tree, before: String = "", after: String = "", indent: Boolean = false): Fragment = {
      if(indent && t != null && t.pos == NoPosition) {
        val child = f(t, ind) getOrElse EmptyFragment
        val childCenter = Layout(child.center.asText.replaceAll("\n", "\n"+ ind.defaultIncrement))
        Fragment(Layout(ind.defaultIncrement) ++ child.leading, childCenter, child.trailing)
      } else
        f(t, ind) getOrElse EmptyFragment
    }
    
    def handleMany(ts: List[Tree], separator: String = "", indent: Boolean = false, after: String = ""): Fragment = {
     (ts match {
        case Nil       => EmptyFragment
        
        case x :: Nil  => handle(x, indent = indent) match {
          case EmptyFragment => EmptyFragment
          case f @ Fragment(l, c, t) if !t.contains(after) => 
            f ++ SeparatedBy(after)
          case f => f
        }
        
        case x :: rest => (handle(x, indent = indent), handleMany(rest, separator, indent = indent, after = after)) match {
          case (l, r) if l.asText == "" => r 
          case (l, r) if l.asText.matches(".*"+ separator +"\\s*$") || r.asText.startsWith(separator) =>
            l ++ r
          case (l, r) =>
            Fragment(
                l.leading, 
                l.center ++ l.trailing  ++ Layout(separator.replace("\n", newline)) ++ r.leading ++ r.center,
                r.trailing )
        }
      })
    }
    
    val originalTree = findOriginalTree(t) getOrElse {
      throw new Exception("original tree not found for: "+ t)
    }
    
   (t, originalTree) match {
            
      case (t @ PackageDef(pid, stats), _) =>
        l ++ handleMany(pid :: stats, separator = "\n") ++ r
      
      case (t @ ClassDef(ModifierTree(mods), name, tparams, impl), orig) =>
        l ++ handleMany(mods) ++ (if(t.symbol.isAnonymousClass) EmptyFragment else handle(NameTree(name) setPos orig.namePosition)) ++ handleMany(tparams) ++ handle(impl) ++ r
        
      case (t @ ModuleDef(ModifierTree(mods), name, impl), orig) =>
        l ++ handleMany(mods) ++ handle(NameTree(name) setPos orig.namePosition) ++ handle(impl) ++ r
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, Nil), _) =>
        l ++ handleMany(params, separator = ",") ++ handleMany(earlyBody) ++ handleMany(parents) ++ handle(self) ++ r
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, body), TemplateExtractor(_, _, _, _, origBody)) =>
        
        val preBody = l ++ handleMany(params, separator = ",", after = ")") ++ handleMany(earlyBody) ++ handleMany(parents) ++ handle(self)
        
        if(origBody.size == 0) {
          //there might be an empty body present:
          val trailingLayout = if(r.matches("(?ms).*\\{.*\\}.*"))
            NoLayout
          else
            r
          
          val x = preBody ++ trailingLayout ++ SeparatedBy(" {"+newline) ++ handleMany(body, separator = "\n", indent = true) ++ SeparatedBy(newline+"}")
          x
        } else {
          preBody ++ SeparatedBy(newline) ++ handleMany(body, separator = "\n", indent = true) ++ r
        }
        
      case (t @ DefDef(ModifierTree(mods), newName, tparams, vparamss, tpt, rhs), orig) =>
        val nameTree = NameTree(t.nameString) setPos orig.namePosition
        l ++ handleMany(mods ::: nameTree :: Nil, separator = " ") ++
          handleMany(tparams) ++ vparamss.map(vparams => handleMany(vparams, ",")).foldLeft(EmptyFragment: Fragment)(_ ++ _) ++ handle(tpt) ++ handle(rhs) ++ r
        
      case (t @ ValDef(ModifierTree(mods), newName, tpt, rhs), orig) =>
        l ++ handleMany(mods ::: (NameTree(newName) setPos orig.namePosition) :: Nil, separator = " ") ++ handle(tpt) ++ handle(rhs) ++ r

      case (BlockExtractor(stats), _) => 
        l ++ handleMany(stats) ++ r
        
      case (t: TypeTree, _) if t.original == null && !t.pos.isTransparent => 
        l ++ Fragment(t.toString) ++ r
        
      case (t: TypeTree, _) => 
        l ++ handle(t.original) ++ r
        
      case (t: AppliedTypeTree, _) => 
        l ++ handle(t.tpt) ++ handleMany(t.args) ++ r
        
      case (t: TypeApply, _) => 
        l ++ handle(t.fun) ++ handleMany(t.args) ++ r
        
      case (t @ TypeDef(ModifierTree(mods), name, tparams, rhs), orig) => 
        l ++ handleMany(mods ::: (NameTree(name) setPos orig.namePosition) :: Nil, separator = " ") ++ handleMany(tparams) ++ handle(rhs)  ++ r
        
      case (t: Ident, _) => 
        l ++ Fragment(t.nameString)  ++ r
        
      // XXX List(..) has an invisible immutable.this qualifier
      case (t @ Select(qualifier: This, selector), _) if qualifier.qual.toString == "immutable" && qualifier.pos == NoPosition => 
        l ++ Fragment(t.symbol.nameString)  ++ r
        
      // skip <init> from constructor calls
      case (t @ Select(qualifier: New, selector), orig) if t.symbol.isConstructor =>
        l ++ handle(qualifier)  ++ r
        
      case (t @ Select(qualifier, selector), orig) =>
        val nameOrig = NameTree(t.nameString) setPos orig.namePosition
        l ++ handle(qualifier) ++ handle(nameOrig)  ++ r
      
      case (t: Literal, _) =>
        l ++ Fragment(t.toString)  ++ r
        
      case (t @ Apply(fun, args @ ((_: Bind) :: ( _: Bind) :: _)), _) =>
        l ++ handle(fun) ++ handleMany(args)  ++ r
        
      case (t @ Apply(fun, args), _) =>
        l ++ handle(fun) ++ handleMany(args, separator = ",")  ++ r
        
      case (t @ Import(expr, _), _) =>
        val ts = t.Selectors()
        if(ts.size > 1) {
          l ++ handle(expr) ++ SeparatedBy("{") ++ handleMany(ts, separator = ", ") ++ SeparatedBy("}")  ++ r
        } else {
          l ++ handle(expr) ++ handleMany(ts, separator = ", ")  ++ r
        }
        
      case (t @ ImportSelectorTree(name, rename), _) =>
        l ++ handle(name) ++ handle(rename)  ++ r
        
      case (t: NameTree, _) =>
        if(t.pos.isTransparent) 
          l ++ EmptyFragment ++ r
        else 
          l ++ Fragment(t.nameString)  ++ r
        
      case (t @ SuperConstructorCall(clazz, args), _) =>
        l ++ handle(clazz) ++ handleMany(args, separator = ",") ++ r
        
      case (t @ SelfTypeTree(name, types, orig), _) =>
        l ++ handle(name) ++ handleMany(types) ++ r
        
      case (t: ModifierTree, _) =>
        l ++ Fragment(t.nameString) ++ r
        
      case (t @ Function(vparams, body), _) =>
        l ++ handleMany(vparams) ++ handle(body) ++ r
        
      case (t @ If(cond, thenp, elsep), _) =>
        l ++ handle(cond) ++ handle(thenp) ++ handle(elsep) ++ r
        
      case (t: This, _) =>
        l ++ Fragment("this") ++ r
        
      case (Return(expr), _) =>
        l ++ handle(expr) ++ r
        
      case (TypeBoundsTree(lo, hi), _) =>
        l ++ handle(lo) ++ handle(hi) ++ r
        
      case (t @ New(tpt), _) =>
        if(t.pos.start > t.pos.point)
          l ++ handle(tpt) ++ r
        else
          l ++ Fragment("new") ++ handle(tpt) ++ r
          
      case (Match(selector, cases), _) =>
        l ++ handle(selector) ++ handleMany(cases) ++ r
        
      case (CaseDef(pat, guard, body), _) =>
        l ++ handle(pat) ++ handle(guard) ++ handle(body) ++ r
        
      case (Bind(name, body), orig) =>
        val nameOrig = NameTree(name) setPos orig.namePosition
        l ++ handle(nameOrig) ++ handle(body) ++ r
        
      case (Typed(expr, tpt), _) =>
        l ++ handle(expr) ++ handle(tpt) ++ r
        
      case (t, _) => 
        println("printWithExistingLayout: "+ t.getClass.getSimpleName)
        l ++ Fragment("ø") ++ r
    }
  }
}