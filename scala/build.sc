import $ivy.`com.lihaoyi::mill-contrib-scoverage:$MILL_VERSION`
import mill._
import mill.api.Loose
import mill.define.Target
import scalalib._
import mill.contrib.scoverage.ScoverageModule

object Deps {
  val utest = ivy"com.lihaoyi::utest::0.8.1"
  // val mockito = ivy"org.mockito::mockito-scala:1.17.14"
}

object lox extends ScoverageModule {
  override def scalaVersion = "3.3.1"

  override def scoverageVersion = "2.0.11"

  override def ivyDeps: Target[Loose.Agg[Dep]] = Agg(
    ivy"com.typesafe.scala-logging::scala-logging:3.9.5",
  )

  trait utest extends ScoverageTests with TestModule.Utest {
    override def moduleDeps: Seq[JavaModule] = super.moduleDeps ++ Seq(lox)

    override def ivyDeps = Agg(
      Deps.utest,
      // Deps.mockito
    )
  }

  object test extends utest

  object integration extends utest

}
