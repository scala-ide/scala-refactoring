package scala.tools.refactoring.tests.util;

import org.junit.Rule;

public abstract class TestRules {

  // all rules need to be public fields

  @Rule
  public final ScalaVersionTestRule rule1 = new ScalaVersionTestRule();

  @Rule
  public final ExceptionWrapper rule2 = new ExceptionWrapper();
}
