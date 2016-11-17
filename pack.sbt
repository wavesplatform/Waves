// Package description
maintainer := "wavesplatform.com"
packageSummary := "Waves node implementation on top of Scorex"
packageDescription := "Waves node"

enablePlugins(JavaServerAppPackaging, JDebPackaging)

import com.typesafe.sbt.packager.archetypes.systemloader._

(serviceAutostart in Debian) := false

mappings in Universal ++= Seq((resourceDirectory in Compile).value / "application-production.conf" -> "conf/application-production.conf",
  (resourceDirectory in Compile).value / "application.conf" -> "conf/application.conf")

import DebianConstants._
maintainerScripts in Debian := maintainerScriptsAppend((maintainerScripts in Debian).value)(
  Postinst -> s"mkdir -p /home/waves && mkdir -p /home/waves/wallet && mkdir -p /home/waves/data && chmod -R 750 /home/waves && chown waves:waves /home/waves"
)
