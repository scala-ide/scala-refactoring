package scala.tools.refactoring
package sourcegen

trait ReusingPrinter {

  self: LayoutHelper with common.Tracing with common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def reuseExistingSource(traverse: (Tree, Indentation) => Option[String], t: Tree, ind: Indentation): String = context("Reuse "+ t.getClass.getSimpleName) { 
    
    val (parentLeadingLayout, parentTrailingLayout) = findOriginalTree(t) map { t =>
    
      val (l, r) = (t.originalLeftSibling, t.originalParent, t.originalRightSibling) match {
        case (_,          None,    _          ) => layoutForCompilationUnitRoot(t)        \\ (_ => trace("compilation unit root"))
        case (None,       Some(p), None       ) => layoutForSingleChild(t, p)             \\ (_ => trace("single child"))
        case (None,       Some(p), Some(right)) => layoutForLeftOuterChild(t, p, right)   \\ (_ => trace("left outer child"))
        case (Some(left), Some(p), None       ) => layoutForRightOuterChild(t, p, left)   \\ (_ => trace("right outer child"))
        case (Some(left), Some(p), Some(right)) => layoutForEnclosedChild(t, left, right) \\ (_ => trace("enclosed child"))
      }
      
      (l.toString, r.toString)
      
    } getOrElse ("", "")
    
    val (originalLeadingLayout, originalTrailingLayout) = originalEnclosingLayout(t)
      
    trace("parent leading:    %s", parentLeadingLayout)
    trace("original leading:  %s", originalLeadingLayout.toString)
    trace("original trailing: %s", originalTrailingLayout.toString)
    trace("parent trailing:   %s", parentTrailingLayout)
        
    val center = printWithExistingLayout(traverse, t, ind)
    
    trace("result:                %s", originalLeadingLayout.toString + center + originalTrailingLayout.toString)
    trace("result with enclosing: %s", parentLeadingLayout + originalLeadingLayout + center + originalTrailingLayout + parentTrailingLayout)
    
    parentLeadingLayout + originalLeadingLayout + center + originalTrailingLayout + parentTrailingLayout 
  }
     
  /**
   * For the given Tree, return a pair of Layout elements that contain
   * the leading and trailing layout of the original tree.
   * */
  private def originalEnclosingLayout(t: Tree): (Layout, Layout) = {
    
    val originalTree = findOriginalTree(t).get //exception is ok for now
    
    children(originalTree) match {
      case Nil =>
        NoLayout → NoLayout
      case c => 
        splitLayoutBetweenParentAndFirstChild(parent = originalTree, child = c.head)._1 →
        splitLayoutBetweenLastChildAndParent(child = c.last, parent = originalTree)._2
    }
  }

  private def printWithExistingLayout(f: (Tree, Indentation) => Option[String], t: Tree, ind: Indentation) = {
    
    implicit val currentFile = t.pos.source
    
    def handle(t: Tree, before: String = "", after: String = "") = f(t, ind) getOrElse ""
    
    def handleMany(ts: List[Tree], separator: String = ""): String = ts match {
      case Nil       => ""
      case x :: Nil  => handle(x)
      case x :: rest => (handle(x), handleMany(rest, separator)) match {
        case (l, r) if l.endsWith(separator) || r.startsWith(separator) => l + r
        case (l, r) => l + separator + r
      }
    }
    
    val originalTree = findOriginalTree(t).getOrElse{
      throw new Exception("original tree not found for: "+ t)
    }
    
   (t, originalTree) match {
            
      case (t @ PackageDef(pid, stats), _) =>
        handle(pid) + handleMany(stats)
      
      case (t @ ClassDef(ModifierTree(mods), name, tparams, impl), orig) =>
        handleMany(mods) + (if(t.symbol.isAnonymousClass) "" else handle(NameTree(name) setPos orig.namePosition)) + handleMany(tparams) + handle(impl)
        
      case (t @ ModuleDef(ModifierTree(mods), name, impl), orig) =>
        handleMany(mods) + handle(NameTree(name) setPos orig.namePosition) + handle(impl)
      
      case (t @ TemplateExtractor(params, earlyBody, parents, self, body), _) =>
        handleMany(params, separator = ", ") + handleMany(earlyBody) + handleMany(parents) + handle(self) + handleMany(body)
        
      case (t @ DefDef(ModifierTree(mods), newName, tparams, vparamss, tpt, rhs), orig) =>
        handleMany(mods ::: (NameTree(newName) setPos orig.namePosition) :: Nil, separator = " ") +
          handleMany(tparams) + vparamss.map(vparams => handleMany(vparams, ", ")).mkString("") + handle(tpt) + handle(rhs)
        
      case (t @ ValDef(ModifierTree(mods), newName, tpt, rhs), orig) =>
        handleMany(mods ::: (NameTree(newName) setPos orig.namePosition) :: Nil, separator = " ") + handle(tpt) + handle(rhs)

      case (BlockExtractor(stats), _) => 
        handleMany(stats)
        
      case (t: TypeTree, _) => 
        handle(t.original)
        
      case (t: AppliedTypeTree, _) => 
        handle(t.tpt) + handleMany(t.args)
        
      case (t: TypeApply, _) => 
        handle(t.fun) + handleMany(t.args)
        
      case (t @ TypeDef(ModifierTree(mods), name, tparams, rhs), orig) => 
        handleMany(mods ::: (NameTree(name) setPos orig.namePosition) :: Nil, separator = " ") + handleMany(tparams) + handle(rhs)
        
      case (t: Ident, _) => 
        t.nameString
        
      // XXX List(..) has an invisible immutable.this qualifier
      case (t @ Select(qualifier: This, selector), _) if qualifier.qual.toString == "immutable" && qualifier.pos == NoPosition => 
        t.symbol.nameString
        
      case (t @ Select(qualifier, selector), orig) =>
        val nameOrig = NameTree(t.symbol.nameString) setPos orig.namePosition
        handle(qualifier) + handle(nameOrig)
      
      case (t: Literal, _) =>
        t.toString
        
      case (t @ Apply(fun, args), _) =>
        handle(fun) + handleMany(args, separator = ", ")
        
      case (t @ Import(expr, _), _) =>
        val ts = t.Selectors()
        handle(expr) + handleMany(ts, separator = ", ")
        
      case (t @ ImportSelectorTree(name, rename), _) =>
        handle(name) + handle(rename)
        
      case (t: NameTree, _) =>
        if(t.pos.isTransparent) 
          ""
        else
          t.nameString
        
      case (t @ SuperConstructorCall(clazz, args), _) =>
        handle(clazz) + handleMany(args, separator = ", ")
        
      case (t @ SelfTypeTree(name, types), _) =>
        handle(name) + handleMany(types)
        
      case (t: ModifierTree, _) =>
        t.nameString
        
      case (t @ Function(vparams, body), _) =>
        handleMany(vparams) + handle(body)
        
      case (t @ If(cond, thenp, elsep), _) =>
        handle(cond) + handle(thenp) + handle(elsep)
        
      case (t: This, _) =>
        "this"
        
      case (Return(expr), _) =>
        handle(expr)
        
      case (TypeBoundsTree(lo, hi), _) =>
        handle(lo) + handle(hi)
        
      case (t @ New(tpt), _) =>
        if(t.pos.start > t.pos.point)
          handle(tpt)
        else
          "new" + handle(tpt)
        
      case (t, _) => 
        println("printWithExistingLayout: "+ t.getClass.getSimpleName)
        "ø"
      
    }
  }
}