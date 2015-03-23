import com.typesafe.sbt.SbtStartScript

import SbtStartScript.StartScriptKeys._

organization := "com.consensusresearch"

name := "scorex"

version := "0.1"

scalaVersion := "2.11.6"

resolvers ++= Seq("Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases/",
                  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

libraryDependencies ++= Seq(
  "com.yandex.yoctodb" % "yoctodb-core" % "0.0.4", //todo: update, last verison is 0.0.8
  "org.mapdb" % "mapdb" % "1.0.7",
  "com.typesafe.play" %% "play-json" % "2.3.4",
  "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  "io.spray" %% "spray-routing" % "1.3.2",
  "io.spray" %% "spray-can" % "1.3.2",
  "io.spray" %% "spray-http" % "1.3.2",
  "io.spray" %% "spray-httpx" % "1.3.2",
  "io.spray" %% "spray-util" % "1.3.2",
  "com.google.guava" % "guava" % "15.0",
  "commons-net" % "commons-net" % "3.3",
  "ch.qos.logback"  %  "logback-classic"   % "1.1.2"
)


lazy val importRun = inputKey[Unit]("custom run task for importing")

importRun := {
    val one = (runMain in Compile).fullInput("migration.ImportUtils").evaluated
}

Seq(SbtStartScript.startScriptForClassesSettings:_*)


