name := "org.scala-refactoring.library"

version := "0.10.0-SNAPSHOT"

scalaVersion := "2.11.8"

moduleName := name.value

organization := "org.scala-refactoring"

crossScalaVersions := Seq("2.10.6", "2.11.7", "2.11.8")

crossVersion := CrossVersion.full

scalacOptions ++= (scalaBinaryVersion.value match {
  case "2.11" => Seq(
    "-deprecation:false",
    "-encoding", "UTF-8",
    "-feature",
    "-language:_",
    "-unchecked",
    "-Xlint",
    "-Xfuture",
    "-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-unused-import",
    "-Ywarn-unused"
  )
  case _ => Seq()
})

unmanagedSourceDirectories in Compile += baseDirectory.value / (scalaBinaryVersion.value match {
  case "2.10" => "src/main/scala-2_10"
  case _      => "src/main/scala-2_11"
})

publishMavenStyle := true

useGpg := true

fork := true

publishTo <<= isSnapshot { isSnapshot =>
  val nexus = "https://oss.sonatype.org"
  if (isSnapshot)
    Some("snapshots" at s"$nexus/content/repositories/snapshots")
  else
    Some("releases"  at s"$nexus/service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

autoAPIMappings := true

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

libraryDependencies ++= Seq(
  "org.scala-lang"  % "scala-compiler"    % scalaVersion.value,
  "com.novocode"    % "junit-interface"   % "0.10"              % "test"
)

parallelExecution in Test := false

// sbt doesn't automatically load the content of the MANIFST.MF file, therefore
// we have to do it here by ourselves Furthermore, the version format in the
// MANIFEST.MF is `x.y.z.qualifier` but we need to replace the `qualifier` part
// with a unique identifier otherwise OSGi can't find out which nightly build
// is newest and therefore not all caches are updated with the correct version
// of a nightly.
packageOptions in Compile in packageBin += {
  val m = Using.fileInputStream(new java.io.File("META-INF/MANIFEST.MF")) { in =>
    val manifest = new java.util.jar.Manifest(in)
    val attr = manifest.getMainAttributes
    val key = "Bundle-Version"
    val versionSuffix = scalaBinaryVersion.value.replace('.', '_')
    val date = new java.text.SimpleDateFormat("yyyyMMddHHmm").format(new java.util.Date)
    val sha = "git rev-parse --short HEAD".!!.trim
    attr.putValue(key, attr.getValue(key).replace("qualifier", s"$versionSuffix-$date-$sha"))
    manifest
  }
  Package.JarManifest(m)
}
