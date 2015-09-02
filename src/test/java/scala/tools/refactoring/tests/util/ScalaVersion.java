package scala.tools.refactoring.tests.util;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface ScalaVersion {
  String matches() default "";
  String doesNotMatch() default "";
}
