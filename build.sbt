organization := "org.consensusresearch"

name := "perma-scorex"

version := "0.1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.consensusresearch" %% "scorex" % "+"
)

mainClass in assembly := Some("scorex.perma.Application")

// Package description
maintainer := "scorex <scorex-dev@groups.io>"
packageSummary := "Permacoin implementation on top of Scorex framework"
packageDescription := "Package for permacoin testnet"

enablePlugins(JavaAppPackaging)

//Debian settings
enablePlugins(DebianPlugin)
linuxPackageMappings in Debian := linuxPackageMappings.value
name in Debian := "PermaScorex"
version in Debian := "0.1.0"
genChanges in Debian := new File("changelog.md")
