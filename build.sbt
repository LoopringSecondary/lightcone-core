import sbt._
import Keys._
import Settings._
import Dependencies._

lazy val core = (project in file("."))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(
    basicSettings,
    libraryDependencies ++= commonDependency)