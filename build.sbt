import com.typesafe.sbt.SbtStartScript

import SbtStartScript.StartScriptKeys._

organization := "com.consensusresearch"

name := "scorex"

version := "0.1"

scalaVersion := "2.11.4"

javaOptions in run += "-Djava.library.path=lib"

resolvers ++= Seq("Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
                  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases/",
                  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
                  "for eu.piotrbuda - scalawebsocket_2.10" at "http://dev.nightlabs.org/maven-repository/repo/",
                  "localrepo" at "file://" + file("repo").getAbsolutePath)

libraryDependencies ++= Seq(
  "commons-net" % "commons-net" % "3.3",
  "ch.qos.logback"  %  "logback-classic"   % "1.1.2",
  "com.typesafe.play" %% "play-json" % "2.3.4",
  "com.typesafe.akka" % "akka-actor_2.10" % "2.3.6",
  "io.spray" %% "spray-routing" % "1.3.2",
  "io.spray" %% "spray-can" % "1.3.2",
  "io.spray" %% "spray-http" % "1.3.2",
  "io.spray" %% "spray-httpx" % "1.3.2",
  "io.spray" %% "spray-util" % "1.3.2",
  "io.spray" %% "spray-client" % "1.3.2",
  "org.mapdb" % "mapdb" % "1.0.6",
  "com.googlecode.json-simple" % "json-simple" % "1.1.1",
  "com.google.guava" % "guava" % "15.0",
  "com.github.briandilley.jsonrpc4j" % "jsonrpc4j" % "1.1",
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

seq(SbtStartScript.startScriptForClassesSettings:_*)

