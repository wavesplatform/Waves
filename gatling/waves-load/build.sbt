import Dependencies._

lazy val root = (project in file("."))
  .settings(
    inThisBuild(
      List(
        organization := "com.wavesplatform",
        scalaVersion := "2.13.8",
        version      := "0.1.0",
      ),
    ),
    name := "waves-load",

    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-Xfatal-warnings",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials",
      "-language:postfixOps",
    ),
  )
