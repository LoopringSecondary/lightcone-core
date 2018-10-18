import sbt._

object Dependencies {
  lazy val commonDependency = Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % Test,
    "org.slf4j" % "slf4j-api" % "1.7.25")
}