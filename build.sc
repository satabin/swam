import mill._
import mill.eval._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._

import ammonite.ops._
import mill.modules.Jvm.runSubprocess

import coursier.maven.MavenRepository

import $file.jmh
import jmh.Jmh
import $file.headers
import headers.Headers

import $file.mdoc
import mdoc.MdocModule

import $ivy.`com.lihaoyi::mill-contrib-bloop:0.3.6-85-38fce6`

val swamVersion = "0.2.0"

val swamLicense = License.`Apache-2.0`

val swamUrl = "https://github.com/satabin/swam"

val swamDeveloper = Developer("satabin", "Lucas Satabin", "https://github.com/satabin")

  val pureconfigVersion = "0.10.2"

trait SwamModule extends ScalaModule with ScalafmtModule with Headers {

  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
    MavenRepository("https:/dl.bintray.com/tpolecat/maven"))

  def scalaVersion = "2.12.8"

  def scalacOptions = Seq("-feature", "-deprecation", "-unchecked", "-Ypartial-unification")

  def scalacPluginIvyDeps = Agg(
    ivy"org.scalamacros:::paradise:2.1.1",
    ivy"org.spire-math::kind-projector:0.9.7",
    ivy"com.olegpy::better-monadic-for:0.3.0-M4")

}

object core extends SwamModule with PublishModule {

  def ivyDeps = Agg(
    ivy"com.beachape::enumeratum:1.5.13",
    ivy"co.fs2::fs2-core:1.0.4",
    ivy"org.scodec::scodec-stream:1.2.1",
    ivy"com.github.pureconfig::pureconfig-generic:$pureconfigVersion",
    ivy"com.github.pureconfig::pureconfig-squants:$pureconfigVersion",
    ivy"com.github.pureconfig::pureconfig-cats-effect:$pureconfigVersion",
    ivy"org.scodec::scodec-core:1.11.3")

  def publishVersion = swamVersion

  def artifactName = "swam-core"

  def pomSettings = PomSettings(
    description = "Swam core library to manipulate WebAssembly programs",
    organization = "org.gnieh",
    url = swamUrl,
    licenses = Seq(swamLicense),
    versionControl = VersionControl.github("satabin", "swam"),
    developers = Seq(swamDeveloper))

  object test extends Tests with ScalafmtModule {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.7")
    def testFrameworks = Seq("swam.util.Framework")
    def moduleDeps = Seq(core, util.test)
  }

}

object text extends SwamModule with PublishModule {
  def moduleDeps = Seq(core)

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.1.2",
    ivy"co.fs2::fs2-io:1.0.4")

  def publishVersion = swamVersion

  def artifactName = "swam-text"

  def pomSettings = PomSettings(
    description = "Swam text library to parse and compile text format",
    organization = "org.gnieh",
    url = swamUrl,
    licenses = Seq(swamLicense),
    versionControl = VersionControl.github("satabin", "swam"),
    developers = Seq(swamDeveloper))

}

object runtime extends SwamModule with PublishModule {

  def moduleDeps = Seq(core)

  def publishVersion = swamVersion

  def artifactName = "swam-runtime"

  def pomSettings = PomSettings(
    description = "Swam runtime library to run WebAssembly programs",
    organization = "org.gnieh",
    url = swamUrl,
    licenses = Seq(swamLicense),
    versionControl = VersionControl.github("satabin", "swam"),
    developers = Seq(swamDeveloper))

  object test extends Tests with ScalafmtModule {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.7",
      ivy"com.github.pathikrit::better-files:3.7.1",
      ivy"com.lihaoyi::pprint:0.5.4")
    def testFrameworks = Seq("swam.util.Framework")
    def moduleDeps = Seq(runtime, text, util.test)
  }

}

object examples extends SwamModule with MdocModule {

  def moduleDeps = Seq(runtime, text)

  def mdocVersion = "1.2.10"

  def mdocSite = Map("VERSION" -> swamVersion)

  def mdocTargetDirectory = os.pwd / 'site / 'content / 'examples

}

object util extends SwamModule {

  object test extends SwamModule {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.7",
      ivy"com.github.pathikrit::better-files:3.7.1")
  }

}

object benchmarks extends SwamModule with Jmh {

  def moduleDeps = Seq(runtime)

  def millSourcePath = pwd / "benchmarks"

}

def unidoc(ev: Evaluator) = T.command {

  def isInfra(x: ScalaModule): Boolean =
    x match {
      case x: ScalaModule#Tests => true
      case _ =>
        val segments = x.millModuleBasePath.value.segments.toVector
        segments.contains("util") || segments.contains("benchmarks") || segments.contains("examples")
    }

  val modules = ev.rootModule.millInternal.segmentsToModules.values
      .collect{ case x: ScalaModule if !isInfra(x) => x}
      .toSeq
  val base = ev.rootModule.millModuleBasePath.value.toNIO.toString

  val sources = ev.evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.allSources): _*)).values.collect {
    case paths: Seq[PathRef] => paths
  }.flatten

  val javadocDir = os.pwd / 'site / 'content / 'api
  mkdir(javadocDir)

  val files = for{
    ref <- sources
    if exists(ref.path)
    p <- (if (ref.path.isDir) ls.rec(ref.path) else Seq(ref.path))
    if (p.isFile && ((p.ext == "scala") || (p.ext == "java")))
  } yield p.toNIO.toString

  val pluginOptions = ev.evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.scalacPluginClasspath): _*)).values.collect {
    case a: Agg[_] => a.items.collect {
      case p: PathRef => s"-Xplugin:${p.path}"
    }
  }.flatten.distinct

  val scalacOptions = ev.evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.scalacOptions): _*)).values.collect {
    case l: List[_] => l.collect {
      case s: String => s
    }
  }.flatten.distinct

  def url(v: String): String = {
    val branch = if (v.endsWith("SNAPSHOT")) "master" else v
    "http://github.com/satabin/swam/tree/" + branch
  }

  val urlString = s"${url(swamVersion)}/€{FILE_PATH}.scala#L1"

  val options = Seq("-d", javadocDir.toNIO.toString, "-usejavacp", "-doc-title", "Swam API Documentation", "-doc-version", swamVersion, "-skip-packages", "fastparse", "-doc-source-url", urlString, "-sourcepath", base) ++ pluginOptions ++ scalacOptions

  val scalaCompilerClasspath = ev.evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.scalaCompilerClasspath): _*)).values.collect {
    case a: Agg[_] => a.items.collect {
      case p: PathRef => p.path
    }
  }.flatten

  val compileClasspath = ev.evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.compileClasspath): _*)).values.collect {
    case a: Agg[_] => a.items.collect {
      case p: PathRef => p
    }
  }.flatten

  if (files.nonEmpty) runSubprocess(
    "scala.tools.nsc.ScalaDoc",
    scalaCompilerClasspath ++ compileClasspath.filter(_.path.ext != "pom").map(_.path),
    mainArgs = (files ++ options).toSeq
  )

  ()
}
