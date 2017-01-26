import com.typesafe.config.ConfigFactory
import sbt.Keys._

val appConf = ConfigFactory.parseFile(new File("src/main/resources/application.conf")).resolve().getConfig("app")

name in ThisBuild := "waves"

organization in ThisBuild := "com.wavesplatform"

version in ThisBuild := appConf.getString("version")

scalaVersion in ThisBuild := "2.12.1"

scalacOptions in ThisBuild += "-target:jvm-1.8"
scalacOptions ++= Seq("-feature", "-deprecation", "-Xmax-classfile-name", "128")

javaOptions ++= Seq(
  "-server", "-Xmx1G"
)

lazy val commonSettings = Seq(
  organization := "com.wavesplatform",
  scalaVersion := "2.12.1"
)

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

libraryDependencies ++=
  Dependencies.db ++
    Dependencies.http ++
    Dependencies.akka ++
    Dependencies.serialization ++
    Dependencies.testKit ++
    Dependencies.logging ++
    Dependencies.p2p ++ Seq(
    "org.scorexfoundation" %% "scrypto" % "1.2.0",
    "commons-net" % "commons-net" % "3.+",
    "com.github.pathikrit" %% "better-files" % "2.17.+"
//    "org.typelevel" %% "cats" % "0.8.1",

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

test in assembly := {}

(dependencyClasspath in Test) <<= (dependencyClasspath in Test).map(
  _.filterNot(_.data.name.contains("logback"))
)
