/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests

import scala.tools.refactoring.implementations.ExplicitGettersSetters
import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.SilentTracing
import scala.tools.refactoring.analysis.FullIndexes
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class ExplicitGettersSettersTest extends TestHelper with TestRefactoring {
  outer =>
  
  def explicitGettersSetters(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExplicitGettersSetters with Tracing {
      val global = outer.global
      pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) foreach ( index processTree _ )
      
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters)
  }.changes
  
  @Test
  def oneVarFromMany = new FileSet {
    add(
    """
      package oneFromMany
      class Demo(val a: String,  /*(*/var i: Int/*)*/  ) {
        def doNothing = () 
      }
    """,
    """
      package oneFromMany
      class Demo(val a: String,  /*(*/_i: Int/*)*/  ) {
        def i = {
          _i
        }
        def i_=(i: Int) = {
          _i = i
        }
        def doNothing = () 
      }
    """)
  } applyRefactoring(explicitGettersSetters)

  @Test
  def oneValFromMany = new FileSet {
    add(
    """
      package oneFromMany
      class Demo(val a: String,  /*(*/val i: Int/*)*/  ) {
        def doNothing = () 
      }
    """,
    """
      package oneFromMany
      class Demo(val a: String,  /*(*/_i: Int/*)*/  ) {
        def i = {
          _i
        }
        def doNothing = () 
      }
    """)
  } applyRefactoring(explicitGettersSetters)
}
