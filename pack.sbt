// Package description
maintainer := "wavesplatform.com"
packageSummary := "Waves node implementation on top of Scorex"
packageDescription := "Waves node"

enablePlugins(JavaAppPackaging)

//Debian settings
enablePlugins(DebianPlugin)
linuxPackageMappings in Debian := linuxPackageMappings.value
name in Debian := name.value
version in Debian := version.value
genChanges in Debian := new File("release-notes.md")

name in Universal := name.value
//
//name in UniversalDocs <<= name in Universal
//
//name in UniversalSrc <<= name in Universal
//
//packageName in Universal := packageName.value