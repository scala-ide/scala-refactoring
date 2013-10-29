/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import common.Selections

/**
 * Provides some simple methods to analyze the program's
 * data flow, as used by Extract Method to find in and out
 * parameters.
 */
trait TreeAnalysis {

  this: Selections with Indexes with common.CompilerAccess =>

  /**
   * From the selection and in the scope of the currentOwner, returns
   * a list of all symbols that are owned by currentOwner and used inside
   * but declared outside the selection.
   */
  def inboundLocalDependencies(selection: Selection, currentOwner: global.Symbol): List[global.Symbol] = {

    val allLocalSymbols = selection.selectedSymbols filter {
      _.ownerChain.contains(currentOwner)
    }

    allLocalSymbols.filterNot {
      index.declaration(_).map(selection.contains) getOrElse true
    }.filter(t => t.pos.isOpaqueRange).sortBy(_.pos.start).distinct
  }

  /**
   * Returns a list of all symbols that are used inside but declared
   * outside the selection.
   */
  def inboundDependencies(selection: Selection): List[global.Symbol] = {
    (for {
      selected <- selection.selectedSymbols
      declaration <- index.declaration(selected)
      if !selection.pos.includes(declaration.pos)
    } yield selected)
      .distinct
  }

  /**
   * From the selection and in the scope of the currentOwner, returns
   * a list of all symbols that are defined inside the selection and
   * used outside of it.
   */
  def outboundLocalDependencies(selection: Selection): List[global.Symbol] = {

    val declarationsInTheSelection = selection.selectedSymbols filter (s => index.declaration(s).map(selection.contains) getOrElse false)

    val occurencesOfSelectedDeclarations = declarationsInTheSelection flatMap (index.occurences)

    occurencesOfSelectedDeclarations.filterNot(selection.contains).map(_.symbol).distinct
  }
}
