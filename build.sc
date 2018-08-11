import mill._
import mill.eval._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._

import ammonite.ops._
import mill.modules.Jvm.subprocess

import coursier.maven.MavenRepository

val swamVersion = "0.1.0-SNAPSHOT"

trait SwamModule extends ScalaModule with ScalafmtModule {

  def repositories = super.repositories ++ Seq(
		MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
		MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
		MavenRepository("https:/dl.bintray.com/tpolecat/maven"))

  def scalaVersion = "2.12.6"

  def scalacOptions = Seq("-feature", "-deprecation", "-unchecked", "-Ypartial-unification", "-Ypatmat-exhaust-depth", "40")

  def scalacPluginIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.7")

}

object core extends SwamModule {

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:1.0.0",
    ivy"com.beachape::enumeratum:1.5.13",
    ivy"org.typelevel::cats-effect:1.0.0-RC2",
    ivy"co.fs2::fs2-core:0.10.5",
    ivy"co.fs2::fs2-io:0.10.5",
    ivy"org.scodec::scodec-stream:1.1.0",
    ivy"org.scodec::scodec-core:1.10.3")

  object test extends Tests with ScalafmtModule {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.4")
    def testFrameworks = Seq("swam.util.Framework")
    def moduleDeps = Seq(core, util.test)
  }

}

object runtime extends SwamModule {

  def moduleDeps = Seq(core)

  def ivyDeps = Agg(
    ivy"com.typesafe:config:1.3.2")

  object test extends Tests with ScalafmtModule {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.4",
      ivy"com.github.pathikrit::better-files:3.5.0",
      ivy"com.lihaoyi::pprint:0.5.3")
    def testFrameworks = Seq("swam.util.Framework")
    def moduleDeps = Seq(runtime, util.test)
  }

}

object util extends SwamModule {

  object test extends SwamModule {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.4",
      ivy"org.scalamacros:::paradise:2.1.1",
      ivy"com.github.pathikrit::better-files:3.5.0")
  }

}

def unidoc(ev: Evaluator[Any]) = T.command {
  val modules = ev.rootModule.millInternal.segmentsToModules.values
      .collect{ case x: ScalaModule if !x.isInstanceOf[ScalaModule#Tests] && !x.millModuleBasePath.value.segments.contains("util") => x}
      .toSeq
  val outDir = T.ctx().dest
  val base = ev.rootModule.millModuleBasePath.value.toNIO.toString

  val sources = ev.evaluate(mill.util.Strict.Agg[define.Task[_]](modules.map(_.allSources): _*)).values.collect {
    case paths: Seq[PathRef] => paths
  }.flatten

  val javadocDir = outDir / 'javadoc
  mkdir(javadocDir)

  val files = for{
    ref <- sources
    if exists(ref.path)
    p <- (if (ref.path.isDir) ls.rec(ref.path) else Seq(ref.path))
    if (p.isFile && ((p.ext == "scala") || (p.ext == "java")))
  } yield p.toNIO.toString

  val pluginOptions = ev.evaluate(mill.util.Strict.Agg[define.Task[_]](modules.map(_.scalacPluginClasspath): _*)).values.collect {
    case a: Agg[_] => a.items.collect {
      case p: PathRef => s"-Xplugin:${p.path}"
    }
  }.flatten.distinct

  val scalacOptions = ev.evaluate(mill.util.Strict.Agg[define.Task[_]](modules.map(_.scalacOptions): _*)).values.collect {
    case l: List[_] => l.collect {
      case s: String => s
    }
  }.flatten.distinct

  def url(v: String): String = {
    val branch = if (v.endsWith("SNAPSHOT")) "master" else v
    "http://github.com/satabin/swam/tree/" + branch
  }

  val urlString = s"${url(swamVersion)}/€{FILE_PATH}.scala"

  val options = Seq("-d", javadocDir.toNIO.toString, "-usejavacp", "-doc-title", "Swam API Documentation", "-doc-version", swamVersion, "-skip-packages", "fastparse", "-doc-source-url", urlString, "-sourcepath", base) ++ pluginOptions ++ scalacOptions

  val scalaCompilerClasspath = ev.evaluate(mill.util.Strict.Agg[define.Task[_]](modules.map(_.scalaCompilerClasspath): _*)).values.collect {
    case a: Agg[_] => a.items.collect {
      case p: PathRef => p.path
    }
  }.flatten

  val compileClasspath = ev.evaluate(mill.util.Strict.Agg[define.Task[_]](modules.map(_.compileClasspath): _*)).values.collect {
    case a: Agg[_] => a.items.collect {
      case p: PathRef => p
    }
  }.flatten

  if (files.nonEmpty) subprocess(
    "scala.tools.nsc.ScalaDoc",
    scalaCompilerClasspath ++ compileClasspath.filter(_.path.ext != "pom").map(_.path),
    mainArgs = (files ++ options).toSeq
  )

  ()
}
