/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring.tests.util

import org.junit.After
import scala.tools.refactoring.util.CompilerInstance

trait FreshCompilerForeachTest extends TestHelper {

  // We are experiencing instable test runs, maybe it helps when we
  // use a fresh compiler for each test case:
  override val global = (new CompilerInstance).compiler

  @After
  def shutdownCompiler {
    global.askShutdown
  }
}

