/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import tools.nsc.ast.parser.Scanners
import tools.nsc.ast.parser.Tokens
import scala.util.control.NonFatal
import scala.reflect.internal.util.BatchSourceFile

/**
 * NameValidation offers several methods to validate
 * new names; depending on the context they are used.
 */
trait NameValidation {

  self: Indexes with common.Selections with common.CompilerAccess =>

  import global._

  /**
   * Returns true if this name is a valid identifier,
   * as accepted by the Scala compiler.
   */
  def isValidIdentifier(name: String): Boolean = {

    val scanner = new { val global = self.global } with Scanners {
      val cu = new global.CompilationUnit(new BatchSourceFile("", name))
      val scanner = new UnitScanner(cu)
    }.scanner

    try {
      scanner.init()
      val firstTokenIsIdentifier = Tokens.isIdentifier(scanner.token)

      scanner.nextToken()
      val secondTokenIsEOF = scanner.token == Tokens.EOF

      firstTokenIsIdentifier && secondTokenIsEOF
    } catch {
      case NonFatal(_) => false
    }
  }

  /**
   * Returns all symbols that might collide with the new name
   * at the given symbol's location.
   *
   * For example, if the symbol is a method, it is checked if
   * there already exists a method with this name in the full
   * class hierarchy of that method's class.
   *
   * The implemented checks are only an approximation and not
   * necessarily correct.
   */
  def doesNameCollide(name: String, s: Symbol): List[Symbol] = {

    def isNameAlreadyUsedInLocalScope: List[Symbol] = {
      (index declaration s.owner map TreeSelection).toList flatMap {
        _.selectedSymbols.filter(_.nameString == name)
      }
    }

    def isNameAlreadyUsedInClassHierarchy = {
      index completeClassHierarchy s.owner flatMap (_.tpe.members) filter (_.nameString == name)
    }

    def isNameAlreadyUsedInPackageHierarchy = {
      index completePackageHierarchy s.owner flatMap (_.tpe.members) filter (_.nameString == name)
    }

    val owner = s.owner

    if(s.isPrivate || s.isLocal) {
      isNameAlreadyUsedInLocalScope.distinct
    } else if(owner.isClass && !(owner.isModuleClass || owner.isClass && nme.isLocalName(owner.name))) {
      isNameAlreadyUsedInClassHierarchy.distinct
    } else {
      isNameAlreadyUsedInPackageHierarchy.distinct
    }
  }
}
