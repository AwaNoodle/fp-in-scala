import mill._, mill.scalalib._, scalafmt._

object fp_in_scala extends SbtModule with ScalafmtModule {
  def scalaVersion = "2.13.5"

  object test extends Tests with TestModule.ScalaTest { 
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.8")
  }
}
