import com.typesafe.sbt.packager.debian.DebianPlugin.autoImport.DebianConstants._

enablePlugins(RunApplicationSettings, ExtensionPackaging)

resolvers += "dnvriend" at "http://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.matcher

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "DEX",
  packageDescription := "Decentralized EXchange for Waves network"
)

packageSettings
inScope(Global)(packageSettings)

Debian / maintainerScripts := maintainerScriptsAppend((Debian / maintainerScripts).value - Postrm)(
  Postrm ->
    s"""#!/bin/sh
       |set -e
       |if [ "$$1" = purge ]; then
       |  rm -rf /var/lib/${nodePackageName.value}/matcher
       |fi""".stripMargin
)
