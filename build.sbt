import com.typesafe.sbt.SbtStartScript

import SbtStartScript.StartScriptKeys._

organization := "com.consensusresearch"

name := "scorex"

version := "0.1"

scalaVersion := "2.11.6"

resolvers ++= Seq("Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases/",
                  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

libraryDependencies ++= Seq(
  "com.yandex.yoctodb" % "yoctodb-core" % "0.0.4",
  "commons-net" % "commons-net" % "3.3",
  "ch.qos.logback"  %  "logback-classic"   % "1.1.2",
  "com.typesafe.play" %% "play-json" % "2.3.4",
  "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  "io.spray" %% "spray-routing" % "1.3.2",
  "io.spray" %% "spray-can" % "1.3.2",
  "io.spray" %% "spray-http" % "1.3.2",
  "io.spray" %% "spray-httpx" % "1.3.2",
  "io.spray" %% "spray-util" % "1.3.2",
  "io.spray" %% "spray-client" % "1.3.2",
  "org.mapdb" % "mapdb" % "1.0.6",
  "com.google.guava" % "guava" % "15.0",
  "org.eclipse.jetty" % "jetty-server" % "9.2.6.v20141205",
  "org.eclipse.jetty" % "jetty-servlet" % "9.2.6.v20141205",
  "org.glassfish.jersey.containers" % "jersey-container-servlet-core" % "2.14",
  "org.glassfish.jersey.core" % "jersey-server" % "2.14",
  "commons-net" % "commons-net" % "3.3",
  "javax.ws.rs" % "javax.ws.rs-api" % "2.0.1"
)


lazy val importRun = inputKey[Unit]("custom run task for importing")

importRun := {
    val one = (runMain in Compile).fullInput("migration.ImportUtils").evaluated
}

Seq(SbtStartScript.startScriptForClassesSettings:_*)


