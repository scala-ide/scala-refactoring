package scala.tools.refactoring.tests.util;

import org.junit.Rule;
import org.junit.rules.MethodRule;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.Statement;
import scala.util.Properties;
import org.junit.Assume;

abstract public class ScalaVersionTestRule {

  final class EmptyStatement extends Statement {
    @Override
    public void evaluate() throws Throwable {
      Assume.assumeTrue(false);
    }
  }

  @Rule
  public MethodRule rule = new MethodRule() {

    public Statement apply(Statement stmt, FrameworkMethod meth, Object arg2) {
      ScalaVersion onlyOn = meth.getAnnotation(ScalaVersion.class);

      if (onlyOn != null) {
        if (Properties.versionString().contains(onlyOn.matches())) {
          return stmt;
        } else {
          return new EmptyStatement();
        }
      } else {
        return stmt;
      }
    }
  };
}
