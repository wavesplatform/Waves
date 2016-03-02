organization := "org.consensusresearch"

name := "perma-scorex"

version := "0.1.0"

scalaVersion := "2.11.7"

resolvers += "SonaType" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies ++= Seq(
  "org.consensusresearch" %% "scorex" % "1.2.0"
)

mainClass in assembly := Some("scorex.perma.Application")