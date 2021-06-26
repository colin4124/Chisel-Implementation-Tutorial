// play-chisel/build.sc
import ammonite.ops._
import mill._
import mill.scalalib._

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.12.13"
  def scalacOptions = Seq(
    "-language:reflectiveCalls", // reflective access of structural type member value
    "-deprecation",  // Emit warning and location for usages of deprecated APIs.
    "-explaintypes", // Explain type errors in more detail.
    "-feature",      // Emit warning and location for usages of features that should
    "-unchecked",    // Enable additional warnings where generated code depends on assumptions.
  )
}

object chisel extends CommonModule {
  object core extends CommonModule {
    def ivyDeps = Agg(
      ivy"org.apache.commons:commons-text:1.8",
    )
  }
  def moduleDeps = Seq(core)
  def millSourcePath = super.millSourcePath / ammonite.ops.up
}
