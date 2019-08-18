package scala.tools.refactoring.tests.util;

import org.junit.Assume;
import org.junit.rules.MethodRule;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.Statement;

import scala.util.Properties;

public class ScalaVersionTestRule implements MethodRule {

  final class EmptyStatement extends Statement {
    @Override
    public void evaluate() throws Throwable {
      Assume.assumeTrue(false);
    }
  }

  public Statement apply(Statement stmt, FrameworkMethod meth, Object arg2) {
    ScalaVersion onlyOn = meth.getAnnotation(ScalaVersion.class);
    String versionNumberString = Properties.versionNumberString();

    if (onlyOn != null) {
      if (!onlyOn.doesNotMatch().isEmpty() && versionNumberString.startsWith(onlyOn.doesNotMatch())) {
        return new EmptyStatement();
      } else if (versionNumberString.startsWith(onlyOn.matches())) {
        return stmt;
      } else {
        return new EmptyStatement();
      }
    } else {
      return stmt;
    }
  }
}
