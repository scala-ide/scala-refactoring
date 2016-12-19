package scala.tools.refactoring
package implementations

import scala.language.reflectiveCalls
import scala.tools.refactoring.common.TracingImpl
import scala.tools.refactoring.implementations.oimports.OrganizeImportsWorker

trait ImportsHelper extends TracingImpl {

  self: common.InteractiveScalaCompiler with analysis.Indexes with transformation.Transformations with transformation.TreeTransformations with common.EnrichedTrees =>

  import global._

  def addRequiredImports(importsUser: Option[Tree], targetPackageName: Option[String]) = traverseAndTransformAll {
    findBestPackageForImports &> transformation[(PackageDef, List[Import], List[Tree]), Tree] {
      case (pkg, existingImports, rest) => {
        val user = importsUser getOrElse pkg
        val targetPkgName = targetPackageName getOrElse pkg.nameString
        val oi = new OrganizeImportsWorker[global.type](global) {
          import participants._
          import regionContext._
          import transformations._
          import treeToolbox._
          object NeededImports extends Participant {
            def doApply(trees: List[Import]) = {
              val externalDependencies = neededImports(user) filterNot { imp =>
                // We don't want to add imports for types that are
                // children of `importsUser`.
                index.declaration(imp.symbol).exists { declaration =>
                  val sameFile = declaration.pos.source.file.canonicalPath == user.pos.source.file.canonicalPath
                  def userPosIncludesDeclPos = user.pos.includes(declaration.pos)
                  sameFile && userPosIncludesDeclPos
                }
              }

              val newImportsToAdd = externalDependencies filterNot {
                case Select(qualifier, name) =>
                  val depPkgStr = importAsString(qualifier)
                  val depNameStr = "" + name

                  trees exists {
                    case Import(expr, selectors) =>
                      val impPkgStr = importAsString(expr)

                      selectors exists { selector =>
                        val selNameStr = "" + selector.name
                        val selRenameStr = "" + selector.rename

                        impPkgStr == depPkgStr && {
                          selector.name == nme.WILDCARD || {
                            selNameStr == depNameStr || selRenameStr == depNameStr
                          }
                        }
                      }
                  }
              }
              val existingStillNeededImports = new recomputeAndModifyUnused(user)(
                List(Region(trees.map { imp => new RegionImport(proto = imp)() })))

              existingStillNeededImports.flatMap { _.imports } ::: SortImports(mkImportTrees(newImportsToAdd, targetPkgName))
            }
          }
        }

        val imports = oi.NeededImports(existingImports).filterNot { imp =>
          val noLongerNeeded = targetPkgName == imp.expr.toString && imp.selectors.size == 1 && {
            // Note that we don't touch imports with multiple selectors here. This limitation,
            // that should not result in any regressions, might be addressed in the future.
            val s = imp.selectors.head
            s.name == s.rename
          }

          noLongerNeeded
        }

        // When we move the whole file, we only want to add imports to the originating package
        pkg copy (stats = imports ::: rest) replaces pkg
      }
    }
  }

}
