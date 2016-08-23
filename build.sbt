import sbt.Keys._

lazy val commonSettings = Seq(
  organization := "com.wavesplatform",
  version := version.value,
  scalaVersion := "2.11.8"
)

def subModule(id: String): Project = Project(id = id, base = file(s"scorex-$id"))

lazy val basics = subModule("basics")
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

lazy val transaction = subModule("transaction")
  .aggregate(basics)
  .dependsOn(basics)
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

lazy val consensus = subModule("consensus")
  .aggregate(basics)
  .dependsOn(basics)
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

lazy val root = Project(id = "scorex", base = file("."))
  .aggregate(basics, transaction, consensus)
  .dependsOn(basics % "compile->compile;test->test", transaction, consensus)
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

name := "scorex"

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

libraryDependencies ++=
  Dependencies.db ++
    Dependencies.http ++
    Dependencies.akka ++
    Dependencies.serialization ++
    Dependencies.testKit ++
    Dependencies.logging

scalacOptions ++= Seq("-feature", "-deprecation", "-Xmax-classfile-name", "128")

javaOptions ++= Seq(
  "-server"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

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

fork in ThisBuild := true

pomIncludeRepository in ThisBuild := { _ => false }

licenses in ThisBuild := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage in ThisBuild := Some(url("https://github.com/wavesplatform/Scorex"))

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
