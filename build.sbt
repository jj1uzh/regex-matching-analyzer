name := "regex-matching-analyzer"
scalaVersion := "2.12.11"

Compile / scalaSource := baseDirectory.value / "src"
Test / scalaSource := baseDirectory.value / "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
scalacOptions ++= Seq("-language:higherKinds")
scalafixOnCompile := true

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

Global / lintUnusedKeysOnLoad := false

ThisBuild / useSuperShell := false
shellPrompt := { _ => s"${scala.Console.MAGENTA}sbt:${name.value}> ${scala.Console.RESET}" }
autoStartServer := false
run / fork := true
run / connectInput := true
Global / cancelable := true
