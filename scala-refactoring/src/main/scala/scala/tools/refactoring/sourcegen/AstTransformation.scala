package scala.tools.refactoring
package sourcegen

trait AstTransformations {
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  val treeCopy = new LazyTreeCopier
  
  implicit def treesToTraversalFunction(tree: Tree): (Tree => Tree) => Tree = (transform: (Tree => Tree)) => {
  
    def transformTrees(trees: List[Tree]): List[Tree] =
      trees mapConserve (transform(_))
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template]
    def transformTypeDefs(trees: List[TypeDef]): List[TypeDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[TypeDef])
    def transformValDef(tree: ValDef): ValDef =
      if (tree.isEmpty) tree else transform(tree).asInstanceOf[ValDef]
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      trees mapConserve (transformValDef(_))
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      treess mapConserve (transformValDefs(_))
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[CaseDef])
    def transformIdents(trees: List[Ident]): List[Ident] =
      trees mapConserve (tree => transform(tree).asInstanceOf[Ident])
    def transformStats(stats: List[Tree]): List[Tree] =
      stats mapConserve (stat => transform(stat)) filter (EmptyTree !=)
    def transformUnit(unit: CompilationUnit) { unit.body = transform(unit.body) }
    def transformModifiers(mods: Modifiers): Modifiers =
      Modifiers(mods.flags, mods.privateWithin, transformTrees(mods.annotations), mods.positions)
  
    tree match {
      case EmptyTree => 
        tree
      
      case PackageDef(pid, stats) =>
        treeCopy.PackageDef(
          tree, transform(pid).asInstanceOf[RefTree], 
          transformStats(stats)
        )
      case ClassDef(mods, name, tparams, impl) =>
          treeCopy.ClassDef(tree, transformModifiers(mods), name,
                            transformTypeDefs(tparams), transformTemplate(impl))
        
      case ModuleDef(mods, name, impl) =>
          treeCopy.ModuleDef(tree, transformModifiers(mods),
                             name, transformTemplate(impl))
        
      case ValDef(mods, name, tpt, rhs) =>
          treeCopy.ValDef(tree, transformModifiers(mods),
                          name, transform(tpt), transform(rhs))
        
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          treeCopy.DefDef(tree, transformModifiers(mods), name,
                          transformTypeDefs(tparams), transformValDefss(vparamss),
                          transform(tpt), transform(rhs))
        
      case TypeDef(mods, name, tparams, rhs) =>
          treeCopy.TypeDef(tree, transformModifiers(mods), name,
                           transformTypeDefs(tparams), transform(rhs))
        
      case LabelDef(name, params, rhs) =>
        treeCopy.LabelDef(tree, name, transformIdents(params), transform(rhs))
      case Import(expr, selectors) =>
        treeCopy.Import(tree, transform(expr), selectors)
      case DocDef(comment, definition) =>
        treeCopy.DocDef(tree, comment, transform(definition))
      case Template(parents, self, body) =>
        treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformStats(body))
      case Block(stats, expr) =>
        treeCopy.Block(tree, transformStats(stats), transform(expr))
      case CaseDef(pat, guard, body) =>
        treeCopy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case Alternative(trees) =>
        treeCopy.Alternative(tree, transformTrees(trees))
      case Star(elem) =>
        treeCopy.Star(tree, transform(elem))
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case UnApply(fun, args) =>
        treeCopy.UnApply(tree, fun, transformTrees(args))
      case ArrayValue(elemtpt, trees) =>
        treeCopy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
      case Function(vparams, body) =>
        treeCopy.Function(tree, transformValDefs(vparams), transform(body))
      case Assign(lhs, rhs) =>
        treeCopy.Assign(tree, transform(lhs), transform(rhs))
      case AssignOrNamedArg(lhs, rhs) =>
        treeCopy.AssignOrNamedArg(tree, transform(lhs), transform(rhs))
      case If(cond, thenp, elsep) =>
        treeCopy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case Match(selector, cases) =>
        treeCopy.Match(tree, transform(selector), transformCaseDefs(cases))
      case Return(expr) =>
        treeCopy.Return(tree, transform(expr))
      case Try(block, catches, finalizer) =>
        treeCopy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
      case Throw(expr) =>
        treeCopy.Throw(tree, transform(expr))
      case New(tpt) =>
        treeCopy.New(tree, transform(tpt))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), transform(tpt))
      case TypeApply(fun, args) =>
        treeCopy.TypeApply(tree, transform(fun), transformTrees(args))
      case Apply(fun, args) =>
        treeCopy.Apply(tree, transform(fun), transformTrees(args))
      case ApplyDynamic(qual, args) =>
        treeCopy.ApplyDynamic(tree, transform(qual), transformTrees(args))
      case Super(qual, mix) =>
        treeCopy.Super(tree, qual, mix)
      case This(qual) =>
        treeCopy.This(tree, qual)
      case Select(qualifier, selector) =>
        treeCopy.Select(tree, transform(qualifier), selector)
      case Ident(name) =>
        treeCopy.Ident(tree, name)
      case Literal(value) =>
        treeCopy.Literal(tree, value)
      case TypeTree() =>
        treeCopy.TypeTree(tree)
      case Annotated(annot, arg) =>
        treeCopy.Annotated(tree, transform(annot), transform(arg))
      case SingletonTypeTree(ref) =>
        treeCopy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        treeCopy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(templ) =>
        treeCopy.CompoundTypeTree(tree, transformTemplate(templ))
      case AppliedTypeTree(tpt, args) =>
        treeCopy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
      case TypeBoundsTree(lo, hi) =>
        treeCopy.TypeBoundsTree(tree, transform(lo), transform(hi)) 
      case ExistentialTypeTree(tpt, whereClauses) =>
        treeCopy.ExistentialTypeTree(tree, transform(tpt), transformTrees(whereClauses))
      case SelectFromArray(qualifier, selector, erasure) =>
        treeCopy.SelectFromArray(tree, transform(qualifier), selector, erasure)
      
      case t => throw new Exception("unhandled tree: "+ t)
    }
  }
  
  val treetransformations = new TreeTransformations[Tree]
  import treetransformations._

  val keepTree = filter {
    case t: Tree => t.pos == NoPosition || t.pos.isRange
  }
  
  val noPosition = transform[Tree, Tree] {
    case t: Tree => t.pos = NoPosition; t
  }
  
  val emptyTree = transform[Tree, Tree] {
    // FIXME also need other empty trees, like emptyValDef
    case _ => EmptyTree
  }
  
  val removeAuxiliaryTrees = topdown(keepTree andThen id orElse emptyTree)
  
  val emptyAllPositions = topdown(noPosition)
}