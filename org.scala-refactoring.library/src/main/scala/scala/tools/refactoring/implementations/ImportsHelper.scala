package scala.tools.refactoring
package implementations

trait ImportsHelper {

  self: common.InteractiveScalaCompiler with analysis.Indexes with transformation.Transformations with transformation.TreeTransformations with common.PimpedTrees =>
  
  import global._
  
  def addRequiredImports(importsUser: Option[Tree], targetPackageName: Option[String]) = traverseAndTransformAll {
    locatePackageLevelImports &> transformation[(PackageDef, List[Import], List[Tree]), Tree] {
      case (pkg, existingImports, rest) => {
        val user = importsUser getOrElse pkg
        val targetPkgName = targetPackageName getOrElse pkg.nameString
        val oi = new OrganizeImports {
          val global: self.global.type = self.global
            
          object NeededImports extends Participant {
            def apply(trees: List[Import]) = {
              
              val imps = neededImports(user) filterNot { imp =>
                // We don't want to add imports for types that are
                // children of `importsUser`.
                val declaration = index.declaration(imp.symbol)
                declaration map (user.pos includes _.pos) getOrElse false
              }
              mkImportTrees(imps, targetPkgName)
            }
          }
        }
        // TODO: Use IDE settings
        val imports = scala.Function.chain(oi.NeededImports :: oi.SortImports :: Nil)(existingImports)         
        // When we move the whole file, we only want to add imports to the originating package
        pkg copy (stats = imports ::: rest) replaces pkg
      }
    }
  }
  
}