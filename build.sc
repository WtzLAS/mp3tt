import mill._, scalalib._

object main extends ScalaModule {
  def scalaVersion = "3.4.1"
  
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.10.0"
  )
}