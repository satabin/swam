import scalariform.formatter.preferences._

lazy val commonSettings = Seq(
  organization := "org.gnieh",
  scalaVersion := "2.12.4",
  resolvers +=
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  headerLicense := Some(HeaderLicense.ALv2("2018", "Lucas Satabin")),
  licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://gnieh.org/satabin/swam")),
  scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Ypartial-unification", "-Ypatmat-exhaust-depth", "40"),
  scalacOptions in (Compile, doc) ++= Seq("-groups"),
  scalariformAutoformat := true,
  scalariformPreferences := {
    scalariformPreferences.value
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentConstructorArguments, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  },
  publishMavenStyle := true,
  publishArtifact in Test := false,
  // The Nexus repo we're publishing to.
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
    ),
  pomIncludeRepository := { x => false },
  pomExtra := (
    <scm>
      <url>https://github.com/satabin/swam</url>
      <connection>scm:git:git://github.com/satabin/swam.git</connection>
      <developerConnection>scm:git:git@github.com:satabin/swam.git</developerConnection>
      <tag>HEAD</tag>
    </scm>
    <developers>
      <developer>
        <id>satabin</id>
        <name>Lucas Satabin</name>
        <email>lucas.satabin@gnieh.org</email>
      </developer>
    </developers>
    <ciManagement>
      <system>travis</system>
      <url>https://travis-ci.org/#!/satabin/swam</url>
    </ciManagement>
    <issueManagement>
      <system>github</system>
      <url>https://github.com/satabin/swam/issues</url>
    </issueManagement>
  ),
  version := "0.1.0")

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    name := "swam")
  .aggregate(core, vm)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "swam-core",
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % "1.5.13",
      "org.scodec" %% "scodec-stream" % "1.1.0",
      "org.scodec" %% "scodec-core" % "1.10.3"))

lazy val vm = project.in(file("vm"))
  .settings(commonSettings)
  .settings(
    name := "swam-vm")
  .dependsOn(core)
