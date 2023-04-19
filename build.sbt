name := "regex-matching-analyzer"
scalaVersion := "2.13.10"

Compile / scalaSource := baseDirectory.value / "src"
Test / scalaSource := baseDirectory.value / "test"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-unused"
)
scalacOptions ++= Seq("-language:higherKinds")

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixOnCompile := true
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "com.beachape" %% "enumeratum" % "1.7.2"

Global / lintUnusedKeysOnLoad := false

ThisBuild / useSuperShell := false
shellPrompt := { _ => s"${scala.Console.MAGENTA}sbt:${name.value}> ${scala.Console.RESET}" }
autoStartServer := false
run / fork := true
run / connectInput := true
Global / cancelable := true
