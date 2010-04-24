package scala.tools.refactoring
package sourcegen

import tools.nsc.util.SourceFile
import common.{Change, PimpedTrees}

trait SourceGen extends PimpedTrees {
  
  self: LayoutHelper =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
 
  val transformations = new Transformations[Tree]
  import transformations._
  
  val isModified = filter {
    case EmptyTree => false
    case t: Tree => t.pos == NoPosition || t.pos.isRange
  }
  
//  class Template(default: String, traverse: Transformation[Tree, String]) {
//    def → (tree: Tree) = {
//      traverse(tree) match {
//        case Some(t) => leftLayout(tree, default) + t
//        case None => "" 
//      }
//    }
//    
//    def ⏎ (ts: List[Tree]) = {
//      this
//    }
//    
//    def ⏎ = {
//      this
//    }
//  }
//  
//  implicit def stringToTemplate(s: String)(implicit traverse: Transformation[Tree, String]) = new Template(s, traverse)

  def generateSourceCode(self: Transformation[Tree, String], t: Tree): String = t.pos match {
    case NoPosition => 
      createNew(self, t)
    case p if p.isRange =>
      createNew(self, t)
  }
  
  def createNew(traverse: Transformation[Tree, String], t: Tree): String = { 
    
    def format(t: Tree, f: String => String) = traverse(t) match {
      case Some(t) => f(t)
      case None => ""
    }
    
    def formatList(ts: List[Tree], f: String => String) = (ts map traverse) flatten match {
      case Nil => ""
      case xs => xs map f mkString ""
    }
    
    t match {
    
      case EmptyTree =>
        ""
          
      case PackageDef(pid, stats) =>
        format     (pid,   "package "+ _ +"\n") + 
        formatList (stats, _ +"\n")
        
      case ClassDef(mods, name, tparams, impl) =>
        //mods.annotations map traverse
        //tparams map traverse
        
        "class "+ name + format(impl, s => s)
        
  //    case ModuleDef(mods, name, impl) =>
  //      mods.annotations map traverse
  //      traverse(impl)
        
      case ValDef(mods, name, tpt, rhs) =>
        //mods.annotations map traverse
        
        "val "+ name.toString + format(tpt, ": "+ _) + format(rhs, " = "+ _)
        
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        //mods.annotations map traverse
        //tparams map traverse
        val params = vparamss map ( _ map traverse flatten) match {
          case Nil => ""
          case paramLists => paramLists map {paramList => "("+ (paramList mkString ", ") +")"} mkString ""
        }
        
        "def "+ name + params + format(tpt, ": "+ _) + format(rhs, " = "+ _)
        
  //    case TypeDef(mods, name, tparams, rhs) =>
  //      mods.annotations map traverse
  //      tparams map traverse
  //      traverse(rhs)
  //      
  //    case LabelDef(name, params, rhs) =>
  //      params map traverse
  //      traverse(rhs)
  //      
  //    case Import(expr, selectors) =>
  //      traverse(expr)
  //      
  //    case Annotated(annot, arg) =>
  //      traverse(annot)
  //      traverse(arg)
        
      case Template(parents, self, body) =>
        parents map traverse
        traverse(self)
        body map traverse flatten match {
          case Nil => ""
          case xs => " {\n"+ (xs mkString "\n") +"\n}"
        }
        
      case Block(stats, expr) =>
        val all = stats ::: expr :: Nil
        if(all.size > 1) {
          "{\n" + formatList(all, _ +"\n") + "}"
        } else {
          format(all.head, s => s)
        }
        
  //    case CaseDef(pat, guard, body) =>
  //      traverse(pat)
  //      traverse(guard)
  //      traverse(body)
  //      
  //    case Alternative(trees) =>
  //      trees map traverse
  //      
  //    case Star(elem) =>
  //      traverse(elem)
  //      
  //    case Bind(name, body) =>
  //      traverse(body)
  //      
  //    case UnApply(fun, args) =>
  //      traverse(fun)
  //      args map traverse
  //      
  //    case ArrayValue(elemtpt, trees) =>
  //      traverse(elemtpt)
  //      trees map traverse
  //      
  //    case Function(vparams, body) =>
  //      vparams map traverse
  //      traverse(body)
  //      
  //    case Assign(lhs, rhs) =>
  //      traverse(lhs)
  //      traverse(rhs)
  //      
  //    case If(cond, thenp, elsep) =>
  //      traverse(cond)
  //      traverse(thenp)
  //      traverse(elsep)
  //      
  //    case Match(selector, cases) =>
  //      traverse(selector)
  //      cases map traverse
  //      
  //    case Return(expr) =>
  //      traverse(expr)
  //      
  //    case Try(block, catches, finalizer) =>
  //      traverse(block)
  //      catches map traverse
  //      traverse(finalizer)
  //      
  //    case Throw(expr) =>
  //      traverse(expr)
  //      
  //    case New(tpt) =>
  //      traverse(tpt)
  //      
  //    case Typed(expr, tpt) =>
  //      traverse(expr)
  //      traverse(tpt)
  //      
  //    case TypeApply(fun, args) =>
  //      traverse(fun)
  //      args map traverse
  //      
  //    case Apply(fun, args) =>
  //      traverse(fun)
  //      args map traverse
  //      
  //    case ApplyDynamic(qual, args) =>
  //      traverse(qual)
  //      args map traverse
  //      
  //    case Super(_, _) =>
  //      ;
  //      
  //    case This(_) =>
  //      ;
  //      
  //    case Select(qualifier, selector) =>
  //      traverse(qualifier)
        
      case Ident(name) =>
        name.toString
        
      case lit: Literal =>
        lit.toString
        
      case tree: TypeTree => 
        tree.tpe match {
          case tpe if tpe == EmptyTree.tpe => ""
          case tpe: ConstantType => tpe.underlying.toString
          case r @ RefinedType(parents, _) =>
            parents map {
              case NamedType(name, _) => name.toString
            } mkString
          case _ => 
            tree.tpe.toString
        }
        
  //    case SingletonTypeTree(ref) =>
  //      traverse(ref)
  //      
  //    case SelectFromTypeTree(qualifier, selector) =>
  //      traverse(qualifier)
  //      
  //    case CompoundTypeTree(templ) =>
  //      traverse(templ)
  //      
  //    case AppliedTypeTree(tpt, args) =>
  //      traverse(tpt)
  //      args map traverse
  //      
  //    case TypeBoundsTree(lo, hi) =>
  //      traverse(lo)
  //      traverse(hi)
  //      
  //    case ExistentialTypeTree(tpt, whereClauses) =>
  //      traverse(tpt)
  //      whereClauses map traverse
  //      
  //    case SelectFromArray(qualifier, selector, erasure) =>
  //      traverse(qualifier)
      
      case t: Tree => 
        "«?"+ t.getClass.getSimpleName +"?»"
    } 
  }
  

//  implicit def pimpedTree(t: Tree) = new {
//    
//  }
//
//  
//  def original: Tree

  
  def generate(tree: Tree) = {

    recursively(isModified)(generateSourceCode)(tree)
    
  }
  
  
  
}