ThisBuild / organization := "aoc21"
ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file(".")).settings(
  name := "advent-of-code-2021",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.3.0",
    "org.typelevel" %% "cats-effect-kernel" % "3.3.0",
    "org.typelevel" %% "cats-effect-std" % "3.3.0",
    "co.fs2" %% "fs2-core" % "3.2.0",
    "co.fs2" %% "fs2-io" % "3.2.0",
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.6" % Test
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges
