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
      printWithExistingLayout(traverse, t, ind)
    } match {
      case c if allLayout contains "\n" => Fragment(fixIndentation(c.asText, t, thisIndentation, ind.text)) 
      case c => c
    }
    
    Fragment(leadingParent, leadingChild ++ center ++ trailingChild toLayout, trailingParent) \\ (r => trace("result: %s", r.toString))
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

  private def printWithExistingLayout(f: (Tree, Indentation) => Option[Fragment], t: Tree, ind: Indentation): Fragment = {
    
    implicit val currentFile = t.pos.source
    
    def handle(t: Tree, before: String = "", after: String = ""): Fragment = f(t, ind) getOrElse EmptyFragment
    
    def handleMany(ts: List[Tree], separator: String = ""): Fragment = ts match {
      case Nil       => EmptyFragment
      case x :: Nil  => handle(x)
      case x :: rest => (handle(x), handleMany(rest, separator)) match {
        case (l, r) if l.asText == "" => r 
        case (l, r) if l.asText.endsWith(separator) || r.asText.startsWith(separator) =>
          l ++ r
        case (l, r) =>
          Fragment(
              l.leading, 
              l.center ++ l.trailing  ++ Layout(separator.replace("\n", "\n"+ ind.text)) ++ r.leading ++ r.center,
              r.trailing )
      }
    }
    
    val originalTree = findOriginalTree(t) getOrElse {
      throw new Exception("original tree not found for: "+ t)
    }
    
   (t, originalTree) match {
            
      case (t @ PackageDef(pid, stats), _) =>
        handleMany(pid :: stats, separator = "\n")
      
      case (t @ ClassDef(ModifierTree(mods), name, tparams, impl), orig) =>
        handleMany(mods) ++ (if(t.symbol.isAnonymousClass) EmptyFragment else handle(NameTree(name) setPos orig.namePosition)) ++ handleMany(tparams) ++ handle(impl)
        
      case (t @ ModuleDef(ModifierTree(mods), name, impl), orig) =>
        handleMany(mods) ++ handle(NameTree(name) setPos orig.namePosition) ++ handle(impl)
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, body), _) =>
        handleMany(params, separator = ",") ++ handleMany(earlyBody) ++ handleMany(parents) ++ handle(self) ++ handleMany(body)
        
      case (t @ DefDef(ModifierTree(mods), newName, tparams, vparamss, tpt, rhs), orig) =>
        handleMany(mods ::: (NameTree(newName) setPos orig.namePosition) :: Nil, separator = " ") ++
          handleMany(tparams) ++ vparamss.map(vparams => handleMany(vparams, ",")).foldLeft(EmptyFragment: Fragment)(_ ++ _) ++ handle(tpt) ++ handle(rhs)
        
      case (t @ ValDef(ModifierTree(mods), newName, tpt, rhs), orig) =>
        handleMany(mods ::: (NameTree(newName) setPos orig.namePosition) :: Nil, separator = " ") ++ handle(tpt) ++ handle(rhs)

      case (BlockExtractor(stats), _) => 
        handleMany(stats)
        
      case (t: TypeTree, _) if t.original == null && !t.pos.isTransparent => 
        Fragment(t.toString)
        
      case (t: TypeTree, _) => 
        handle(t.original)
        
      case (t: AppliedTypeTree, _) => 
        handle(t.tpt) ++ handleMany(t.args)
        
      case (t: TypeApply, _) => 
        handle(t.fun) ++ handleMany(t.args)
        
      case (t @ TypeDef(ModifierTree(mods), name, tparams, rhs), orig) => 
        handleMany(mods ::: (NameTree(name) setPos orig.namePosition) :: Nil, separator = " ") ++ handleMany(tparams) ++ handle(rhs)
        
      case (t: Ident, _) => 
        Fragment(t.nameString)
        
      // XXX List(..) has an invisible immutable.this qualifier
      case (t @ Select(qualifier: This, selector), _) if qualifier.qual.toString == "immutable" && qualifier.pos == NoPosition => 
        Fragment(t.symbol.nameString)
        
      // skip <init> from constructor calls
      case (t @ Select(qualifier: New, selector), orig) if t.symbol.isConstructor =>
        handle(qualifier)
        
      case (t @ Select(qualifier, selector), orig) =>
        val nameOrig = (if(t.symbol == NoSymbol) NameTree(selector) else NameTree(t.symbol.nameString)) setPos orig.namePosition
        handle(qualifier) ++ handle(nameOrig)
      
      case (t: Literal, _) =>
        Fragment(t.toString)
        
      case (t @ Apply(fun, args @ ((_: Bind) :: ( _: Bind) :: _)), _) =>
        handle(fun) ++ handleMany(args)
        
      case (t @ Apply(fun, args), _) =>
        handle(fun) ++ handleMany(args, separator = ",")
        
      case (t @ Import(expr, _), _) =>
        val ts = t.Selectors()
        if(ts.size > 1) {
          handle(expr) ++ SeparatedBy("{") ++ handleMany(ts, separator = ", ") ++ SeparatedBy("}")
        } else {
          handle(expr) ++ handleMany(ts, separator = ", ")
        }
        
      case (t @ ImportSelectorTree(name, rename), _) =>
        handle(name) ++ handle(rename)
        
      case (t: NameTree, _) =>
        if(t.pos.isTransparent) 
          EmptyFragment
        else
          Fragment(t.nameString)
        
      case (t @ SuperConstructorCall(clazz, args), _) =>
        handle(clazz) ++ handleMany(args, separator = ",")
        
      case (t @ SelfTypeTree(name, types, orig), _) =>
        handle(name) ++ handleMany(types)
        
      case (t: ModifierTree, _) =>
        Fragment(t.nameString)
        
      case (t @ Function(vparams, body), _) =>
        handleMany(vparams) ++ handle(body)
        
      case (t @ If(cond, thenp, elsep), _) =>
        handle(cond) ++ handle(thenp) ++ handle(elsep)
        
      case (t: This, _) =>
        Fragment("this")
        
      case (Return(expr), _) =>
        handle(expr)
        
      case (TypeBoundsTree(lo, hi), _) =>
        handle(lo) ++ handle(hi)
        
      case (t @ New(tpt), _) =>
        if(t.pos.start > t.pos.point)
          handle(tpt)
        else
          Fragment("new") ++ handle(tpt)
          
      case (Match(selector, cases), _) =>
        handle(selector) ++ handleMany(cases)
        
      case (CaseDef(pat, guard, body), _) =>
        handle(pat) ++ handle(guard) ++ handle(body)
        
      case (Bind(name, body), orig) =>
        val nameOrig = NameTree(name) setPos orig.namePosition
        handle(nameOrig) ++ handle(body)
        
      case (Typed(expr, tpt), _) =>
        handle(expr) ++ handle(tpt)
        
      case (t, _) => 
        println("printWithExistingLayout: "+ t.getClass.getSimpleName)
        Fragment("ø")
    }
  }
}