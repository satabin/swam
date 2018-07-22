import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._

import coursier.maven.MavenRepository

trait SwamModule extends ScalaModule with ScalafmtModule {

  def repositories = super.repositories ++ Seq(
		MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
		MavenRepository("https://oss.sonatype.org/content/repositories/releases"))

  def scalaVersion = "2.12.6"

  def scalacOptions = Seq("-feature", "-deprecation", "-unchecked", "-Ypartial-unification", "-Ypatmat-exhaust-depth", "40")

  trait SwamTests extends Tests with ScalafmtModule {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.4")
    def testFrameworks = Seq("utest.runner.Framework")
  }

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

  def scalacPluginIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.7")

  object test extends Tests with ScalafmtModule {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.4",
      ivy"com.github.pathikrit::better-files:3.5.0",
      ivy"com.lihaoyi::pprint:0.5.3")
    def testFrameworks = Seq("utest.runner.Framework")
  }

}

object vm extends SwamModule {

  def moduleDeps = Seq(core)

}
