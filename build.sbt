val scala3Version = "3.2.0"

Compile / doc / scalacOptions := Seq("-groups")

val scalaParserCombinatorVersion = "2.1.1"
val scalaTestVersion             = "3.2.13"
val scalaCheckVersion            = "1.17.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "macro11",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorVersion,
      "org.scalactic"          %% "scalactic"                % scalaTestVersion,
      "org.scalatest"          %% "scalatest"                % scalaTestVersion  % "test",
      "org.scalacheck"         %% "scalacheck"               % scalaCheckVersion % "test"
    )
  )
