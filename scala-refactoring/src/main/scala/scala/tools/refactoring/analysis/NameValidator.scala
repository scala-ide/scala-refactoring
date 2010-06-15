package scala.tools.refactoring
package analysis

import scala.tools.nsc.util.BatchSourceFile
import scala.tools.nsc.ast.parser.{Scanners, Tokens}

trait NameValidator {

  self: Indexes with common.Selections =>
  
  val global: scala.tools.nsc.Global
  import global._
  
  def isValidIdentifier(name: String) = {

    val scanner = new { val global = self.global } with Scanners {
      val cu = new global.CompilationUnit(new BatchSourceFile("", name))
      val scanner = new UnitScanner(cu)
    }.scanner
    
    try {
      scanner.init()
      val firstToken = scanner.token
      
      scanner.nextToken()
      val secondToken = scanner.token
      
      Tokens.isIdentifier(firstToken) && secondToken == Tokens.EOF
    } catch {
      case _ => false
    }
  }
  
  def doesNameCollide(name: String, s: Symbol, index: IndexLookup): List[Symbol] = {
    
    def isNameAlreadyUsedInLocalScope: List[Symbol] = {
      (index declaration s.owner map (new TreeSelection(_))).toList flatMap {
        _.selectedSymbols.filter(_.nameString == name)
      } 
    }
    
    def isNameAlreadyUsedInClassHierarchy = {
      index completeClassHierarchy s.owner flatMap (_.tpe.members) filter (_.nameString == name)
    }
    
    def isNameAlreadyUsedInPackageHierarchy = {
      index completePackageHierarchy s.owner flatMap (_.tpe.members) filter (_.nameString == name)
    }
    
    if(s.isPrivate || s.isLocal) {
      isNameAlreadyUsedInLocalScope
    } else if(s.owner.isClass && !s.owner.isClassOfModule) {
      isNameAlreadyUsedInClassHierarchy
    } else {
      isNameAlreadyUsedInPackageHierarchy
    } distinct
  }
}
