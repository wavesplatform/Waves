import com.typesafe.config.ConfigFactory
import sbt.Keys._

val appConf = ConfigFactory.parseFile(new File("src/main/resources/application.conf")).resolve().getConfig("app")

name in ThisBuild := "waves"

organization in ThisBuild := "com.wavesplatform"

version in ThisBuild := appConf.getString("version")

scalaVersion in ThisBuild := "2.11.8"

scalacOptions in ThisBuild += "-target:jvm-1.8"
scalacOptions ++= Seq("-feature", "-deprecation", "-Xmax-classfile-name", "128")

javaOptions ++= Seq(
  "-server", "-Xmx1G"
)

lazy val commonSettings = Seq(
  organization := "com.wavesplatform",
  version := version.value,
  scalaVersion := "2.11.8"
)

// lazy val root = Project(id = "scorex", base = file("."))
//   .settings(commonSettings: _*)
//   .settings(
//     scalacOptions ++= Seq("-feature", "-deprecation", "-Xmax-classfile-name", "128"),
//     testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
//   )

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

val scorexVersion = "1.6.0-SNAPSHOT"

libraryDependencies ++= 
    Dependencies.db ++
    Dependencies.http ++
    Dependencies.akka ++
    Dependencies.serialization ++
    Dependencies.testKit ++
    Dependencies.logging ++ 
    Dependencies.p2p ++ Seq(
    "org.consensusresearch" %% "scrypto" % "1.0.4",
    "commons-net" % "commons-net" % "3.+",
    "com.github.pathikrit" %% "better-files" % "2.13.0",
//    "org.typelevel" %% "cats" % "0.8.1",
    "org.scalatest" %% "scalatest" % "2.+" % "test",
    "org.scalactic" %% "scalactic" % "2.+" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
    "net.databinder.dispatch" %% "dispatch-core" % "+" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test"
)

val akkaV = "2.4.14"
lazy val addAkkaLibs = Seq(
  "com.typesafe.akka" %% "akka-persistence" % akkaV,
  "com.github.dnvriend" %% "akka-persistence-inmemory" % "1.+" % "test",
  "org.iq80.leveldb" % "leveldb" % "0.7",
  "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8")

libraryDependencies ++= addAkkaLibs

fork in ThisBuild := false
fork in Test := false
parallelExecution in ThisBuild := false
parallelExecution in Test := false

testOptions in Test += Tests.Argument("-oIDOF", "-u", "target/test-reports")

concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

//assembly settings
assemblyJarName in assembly := "scorex.jar"

test in assembly := {}

mainClass in assembly := Some("scorex.lagonaki.server.Server")

//publishing settings

licenses in ThisBuild := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage in ThisBuild := Some(url("https://github.com/wavesplatform/Scorex"))

publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

(dependencyClasspath in Test) <<= (dependencyClasspath in Test).map(
  _.filterNot(_.data.name.contains("logback"))
)

pomIncludeRepository in ThisBuild := { _ => false }

pomExtra in ThisBuild :=
  <scm>
    <url>git@github.com:wavesplatform/Scorex.git</url>
    <connection>scm:git@github.com:wavesplatform/Scorex.git</connection>
  </scm>
  <developers>
    <developer>
      <id>kushti</id>
      <name>Alexander Chepurnoy</name>
      <url>http://chepurnoy.org/</url>
    </developer>
    <developer>
      <id>catena2w</id>
      <name>catena</name>
      <url>https://github.com/catena2w</url>
    </developer>
    <developer>
      <id>gagarin55</id>
      <url>https://github.com/gagarin55</url>
    </developer>
    <developer>
      <id>alexeykiselev</id>
      <name>Alexey Kiselev</name>
      <url>https://github.com/alexeykiselev</url>
    </developer>
    <developer>
      <id>asolovyov</id>
      <name>Alexander Solovyov</name>
      <url>https://github.com/asolovyov</url>
    </developer>
  </developers>
