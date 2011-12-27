package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory

abstract class MoveClass extends MultiStageRefactoring with TreeFactory with common.TreeExtractors with common.InteractiveScalaCompiler {
  
  import global._
  
  type PreparationResult = ImplDef
  
  case class TargetPackage(pkg: String)
  
  type RefactoringParameters = TargetPackage
  
  def prepare(s: Selection) = {
    
    def getSingleImplDef: Option[ImplDef] = {
      s.root match {
        case root: PackageDef => 
          topPackageDef(root) match {
            case PackageDef(_, (impl: ImplDef) :: Nil) =>
              Some(impl)
            case _ => None
          }
        case _ => None
      }
    }

    s.findSelectedOfType[ImplDef] orElse getSingleImplDef toRight PreparationError("No class or object selected.")
  }
    
  def perform(selection: Selection, toMove: PreparationResult, parameters: RefactoringParameters): Either[RefactoringError, List[Change]] = {
            
    val targetPackages = parameters.pkg.split("\\.").toList
    val ancestors = ancestorSymbolsDesc(toMove).init
    
    trace("Selected ImplDef: %s, in package %s, move to %s", toMove.nameString, ancestors map (_.nameString) mkString ("."), parameters.pkg)
        
    val findFirstPackageToRename = filter {
      case pkg @ PackageDef(_, stats) if stats contains toMove => 
        true
      case pkg @ PackageDef(pid, _) =>
        val packages = ancestorSymbolsDesc(pkg) map (_.nameString)
        !(targetPackages startsWith packages)
    }
    
    val changePackageDeclaration = transform {
      case pkg @ PackageDef(pid, stats) =>
        
        val surroundingPackages = ancestorSymbolsDesc(pkg).init map (_.nameString)
        
        val newPid = if(targetPackages.startsWith(surroundingPackages)) {
          (targetPackages.drop(surroundingPackages.size))
        } else {
          targetPackages
        }
        
        if(newPid.isEmpty) {
          toMove
        } else {
          pkg copy (pid = Ident(newPid mkString "."), stats = List(toMove)) replaces pkg
        }
    }

    val moveClass = findFirstPackageToRename &> changePackageDeclaration
    
    Right(transformFile(selection.file, moveClass |> topdown(matchingChildren(moveClass))))
  }
}
