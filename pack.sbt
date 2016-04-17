// Package description
maintainer := "tosch <toschdev@gmail.com>"
packageSummary := "Nxt Consensus Implementation for Waves on top of Scorex"
packageDescription := "Package for Waves testnet"

enablePlugins(JavaAppPackaging)

//Debian settings
enablePlugins(DebianPlugin)
linuxPackageMappings in Debian := linuxPackageMappings.value
name in Debian := name.value
version in Debian := "0.0.1"
genChanges in Debian := new File("changelog.md")

name in Universal := name.value
//
//name in UniversalDocs <<= name in Universal
//
//name in UniversalSrc <<= name in Universal
//
//packageName in Universal := packageName.value