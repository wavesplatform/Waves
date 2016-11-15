// Package description
maintainer := "wavesplatform.com"
packageSummary := "Waves node implementation on top of Scorex"
packageDescription := "Waves node"

enablePlugins(JavaServerAppPackaging, JDebPackaging)

import com.typesafe.sbt.packager.archetypes.systemloader._

val loader = Option(System.getProperty("loader"))

mappings in Universal ++= Seq((resourceDirectory in Compile).value / "application-production.conf" -> "conf/application-production.conf",
  (resourceDirectory in Compile).value / "application.conf" -> "conf/application.conf")

if (loader.exists(_ == "upstart")) {
  mappings in Universal ++= Seq((resourceDirectory in Compile).value / "logback-to-file.xml" -> "conf/logback.xml")
} else if (loader.exists(_ == "systemd")) {
  mappings in Universal ++= Seq((resourceDirectory in Compile).value / "logback.xml" -> "conf/logback.xml")
} else {
  throw new IllegalStateException("invalid loader")
}

if (loader.exists(_ == "upstart")) {
  enablePlugins(UpstartPlugin)
} else if (loader.exists(_ == "systemd")) {
  enablePlugins(SystemdPlugin)
} else {
  throw new IllegalStateException("invalid loader")
}

val network = Option(System.getProperty("network"))

mappings in Universal ++= {
  if (network.exists(_ == "mainnet")) {
    Seq((baseDirectory in Compile).value / "waves-mainnet.json" -> "settings.json")
  } else if (network.exists(_ == "testnet")) {
    Seq((baseDirectory in Compile).value / "waves-testnet.json" -> "settings.json")
  } else {
    throw new IllegalStateException("invalid network")
  }
}

import DebianConstants._
maintainerScripts in Debian := maintainerScriptsAppend((maintainerScripts in Debian).value)(
  Postinst -> s"mkdir -p /home/waves && mkdir -p /home/waves/wallet && mkdir -p /home/waves/data && chmod -R 750 /home/waves && chown waves:waves /home/waves"
)
