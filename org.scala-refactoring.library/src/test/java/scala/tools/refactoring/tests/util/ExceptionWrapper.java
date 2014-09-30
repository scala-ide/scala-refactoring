package scala.tools.refactoring.tests.util;

import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

import scala.tools.nsc.util.FailedInterrupt;

/**
 * In case an assertion error is caught and wrapped by another error type (as it
 * is the case for exceptions that are thrown on the compiler thread), we need
 * to manually unwrap them later, in order to let JUnit "see" them.
 */
public final class ExceptionWrapper implements TestRule {

  @Override
  public Statement apply(final Statement base, final Description description) {
    return new Statement() {
      @Override
      public void evaluate() throws Throwable {
        try {
          base.evaluate();
        } catch (FailedInterrupt e) {
          // a FailedInterrupt is thrown when an exception occurs on the compiler thread
          throw e.getCause();
        }
      }
    };
  }
}
