import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.15" % Test
  lazy val reflect = "org.scala-lang" % "scala-reflect" % "2.13.11"
}
