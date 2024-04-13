import mill._, scalalib._

object main extends ScalaModule {
  def scalaVersion = "3.4.1"
  
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.10.0"
  )

  object test extends ScalaTests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.2")
    def testFramework = "utest.runner.Framework"
  }
}