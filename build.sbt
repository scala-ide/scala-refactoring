name := "org.scala-refactoring.library"

version := "0.7.0"

scalaVersion := "2.11.7"

moduleName := name.value

organization := "org.scala-refactoring"

crossScalaVersions := Seq("2.10.5", "2.11.7")

unmanagedSourceDirectories in Compile += baseDirectory.value / (scalaBinaryVersion.value match {
  case "2.10" => "src/main/scala-2_10"
  case _      => "src/main/scala-2_11"
})

publishMavenStyle := true

useGpg := true

fork := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomExtra := (
 <url>http://scala-refactoring.org</url>
  <licenses>
    <license>
      <name>Scala License</name>
      <url>http://www.scala-lang.org/node/146</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:https://github.com/scala-ide/scala-refactoring.git</connection>
    <developerConnection>scm:git:git@github.com:scala-ide/scala-refactoring.git</developerConnection>
    <tag>master</tag>
    <url>https://github.com/scala-ide/scala-refactoring</url>
  </scm>
  <developers>
    <developer>
      <id>misto</id>
      <name>Mirko Stocker</name>
      <email>me@misto.ch</email>
    </developer>
  </developers>)

credentials += Credentials(Path.userHome / ".m2" / "credentials")

libraryDependencies += {
  "org.scala-lang" % "scala-compiler" % scalaVersion.value
}

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

parallelExecution in Test := false
