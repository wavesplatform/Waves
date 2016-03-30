// Package description
maintainer := "scorex <scorex-dev@groups.io>"
packageSummary := "Permacoin implementation on top of Scorex framework"
packageDescription := "Package for permacoin testnet"

enablePlugins(JavaAppPackaging)

//Debian settings
enablePlugins(DebianPlugin)
linuxPackageMappings in Debian := linuxPackageMappings.value
name in Debian := name.value
version in Debian := "1.2.2"
genChanges in Debian := new File("changelog.md")

name in Universal := name.value
//
//name in UniversalDocs <<= name in Universal
//
//name in UniversalSrc <<= name in Universal
//
//packageName in Universal := packageName.value