val scala3Version = "3.2.0"

//val fs2Version        = "3.2.11"
val scalaParserCombinatorVersion = "2.1.1"
val scalaTestVersion             = "3.2.13"
val scalaCheckVersion            = "3.2.13.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "macro11",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
//      "co.fs2"            %% "fs2-core"        % fs2Version,
//      "co.fs2"            %% "fs2-scodec"      % fs2Version,
      "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorVersion,
      "org.scalactic"          %% "scalactic"                % scalaTestVersion,
      "org.scalatest"          %% "scalatest"                % scalaTestVersion  % "test",
      "org.scalatestplus"      %% "scalacheck-1-16"          % scalaCheckVersion % "test"
    )
  )
