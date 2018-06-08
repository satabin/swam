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

  trait SwamTests extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.3")
    def testFrameworks = Seq("utest.runner.Framework")
  }

}

object core extends SwamModule {

  def ivyDeps = Agg(
    ivy"com.beachape::enumeratum:1.5.13",
    ivy"org.scodec::scodec-stream:1.1.0",
    ivy"org.scodec::scodec-core:1.10.3")

}

object runtime extends SwamModule

object vm extends SwamModule {

  def moduleDeps = Seq(runtime, core)

}
