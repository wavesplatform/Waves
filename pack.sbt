import java.io.File
// Package description
maintainer := "wavesplatform.com"
packageName := name.value
packageSummary := "Waves node implementation on top of Scorex"
packageDescription := "Waves node"

enablePlugins(JavaServerAppPackaging, JDebPackaging)

val network = Option(System.getProperty("network")).getOrElse("mainnet")
mappings in Universal ++= {
  if (network == "mainnet") {
    Seq((baseDirectory in Compile).value / "waves-mainnet.json" -> "settings.json")
  } else if (network == "testnet") {
    Seq((baseDirectory in Compile).value / "waves-testnet.json" -> "settings.json")
  } else {
    throw new IllegalStateException("invalid network")
  }
}

javaOptions in Universal ++= Seq(
  "-J-server",
  // JVM memory tuning for 1g ram
  "-J-Xms128m",
  "-J-Xmx1024m",

  // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
  "-J-XX:+UseG1GC",
  "-J-XX:+UseNUMA",
  "-J-XX:+AlwaysPreTouch",

  // may be can't use with jstack and others tools
  "-J-XX:+PerfDisableSharedMem",
  "-J-XX:+ParallelRefProcEnabled",
  "-J-XX:+UseStringDeduplication",

  "-J-Dsun.net.inetaddr.ttl=60",

  s"-DLOG_PATH=/var/log/${packageName.value}/",

  // Use separate configuration file for production environment
  s"-Dconfig.file=/usr/share/${packageName.value}/conf/application-production.conf",

  // Use separate logger configuration file for production environment
  s"-Dlogback.configurationFile=/usr/share/${packageName.value}/conf/logback.xml"
)

maintainerScripts in Debian := maintainerScriptsAppend((maintainerScripts in Debian).value)(
  DebianConstants.Postinst -> s"mkdir -p /home/waves && mkdir -p /home/waves/wallet && mkdir -p /home/waves/data && chmod -R 750 /home/waves && chown waves:waves /home/waves"
)

(mappings in Universal) ++= Seq((resourceDirectory in Compile).value / "application-production.conf" -> "conf/application-production.conf",
  (resourceDirectory in Compile).value / "application.conf" -> "conf/application.conf")

import com.typesafe.sbt.packager.archetypes.systemloader._

addCommandAlias("packageAll", "; clean " +
  "; set serverLoading in Debian := Some(com.typesafe.sbt.packager.archetypes.systemloader.ServerLoader.Systemd)" +
  "; set mappings in Universal += (resourceDirectory in Compile).value / \"logback.xml\" -> \"conf/logback.xml\"" +
  "; packageDebianSystemD " +
  "; clean " +
  "; set serverLoading in Debian := Some(com.typesafe.sbt.packager.archetypes.systemloader.ServerLoader.Upstart)" +
  "; set mappings in Universal += (resourceDirectory in Compile).value / \"logback-to-file.xml\" -> \"conf/logback.xml\"" +
  "; packageDebianUpstart " +
  "; clean" +
  "; packageJar"
  )

lazy val packageDebianUpstart = taskKey[File]("creates deb-upstart package")
lazy val packageDebianSystemD = taskKey[File]("creates deb-systemd package")
lazy val packageJar = taskKey[File]("creates fat jar")

packageDebianUpstart := {
  val output = baseDirectory.value / "package" / s"waves-$network-upstart-${version.value}.deb"
  val debianFile = (packageBin in Debian).value
  IO.move(debianFile, output)
  output
}

packageDebianSystemD := {
  val output = baseDirectory.value / "package" / s"waves-$network-systemd-${version.value}.deb"
  val debianFile = (packageBin in Debian).value
  IO.move(debianFile, output)
  output
}

packageJar := {
  val output = baseDirectory.value / "package" / s"waves-${version.value}.jar"
  val jarFile = (assembly in Compile).value
  IO.move(jarFile, output)
  output
}
