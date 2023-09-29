import sbt._

object Dependencies {
  val scalatest = "org.scalatest" %% "scalatest" % "3.2.15" % "test"
  val enumeration = Seq(
    "com.beachape" %% "enumeratum" % "1.7.3"
  )
}
