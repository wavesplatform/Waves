

lazy val commonSettings = Seq(
  organization := "org.consensusresearch",
  version := "1.2.5",
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
  .dependsOn(basics, transaction, consensus)
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
    Dependencies.serizalization ++
    Dependencies.testKit ++
    Dependencies.logging

scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq(
  "-server"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

//assembly settings
assemblyJarName in assembly := "scorex.jar"

test in assembly := {}

mainClass in assembly := Some("scorex.lagonaki.server.Server")


//publishing settings

licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage := Some(url("https://github.com/ConsensusResearch/Scorex-Lagonaki"))

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

fork := true

pomIncludeRepository := { _ => false }

licenses in ThisBuild := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage in ThisBuild := Some(url("https://github.com/ConsensusResearch/Scorex-Lagonaki"))

pomExtra in ThisBuild :=
  <scm>
    <url>git@github.com:ConsensusResearch/Scorex-Lagonaki.git</url>
    <connection>scm:git:git@github.com:ConsensusResearch/Scorex-Lagonaki.git</connection>
  </scm>
    <developers>
      <developer>
        <id>kushti</id>
        <name>Alexander Chepurnoy</name>
        <url>http://chepurnoy.org/</url>
      </developer>
    </developers>
