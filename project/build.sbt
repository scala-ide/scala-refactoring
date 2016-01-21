addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

// We need to be able to not add the scoverage plugin to the scala-refactoring build.
// This is necessary because scala-refactoring is built during Scala PR CI, which means
// that we can not rely on any plugins that depend on scalac. For normal scala-refactoring
// builds this variable should not be set, the Scala PR CI however nedes to set it.
if (sys.env.contains("OMIT_SCOVERAGE_PLUGIN"))
  Nil
else
  List(addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5"))
