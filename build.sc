import mill._, mill.scalalib._, scalafmt._

trait CommonModule extends SbtModule with ScalafmtModule {
  def scalaVersion = "2.13.5"

  object test extends Tests { 
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.8")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object fp_in_scala extends CommonModule
