organization := "org.consensusresearch"

name := "lagonaki"

version := "1.2.1"

scalaVersion := "2.11.8"

resolvers += "SonaType" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies ++= Seq(
  "org.consensusresearch" %% "scorex" % "1.+"
)

mainClass in assembly := Some("scorex.perma.Application")