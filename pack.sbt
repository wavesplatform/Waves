import com.typesafe.sbt.SbtNativePackager.autoImport._
import sbt.Keys._

addCommandAlias("packageAll", "; clean " +
  "; systemd/packageDebianSystemd " +
  "; clean " +
  "; upstart/packageDebianUpstart " +
  "; clean" +
  "; packageJar"
)

val network = Option(System.getProperty("network")).getOrElse("mainnet")

// see https://github.com/muuki88/sbt-native-packager-examples/blob/master/multiple-package-outputs/build.sbt and https://github.com/muuki88/sbt-native-packager-examples/blob/master/multi-module-build/build.sbt
lazy val packageDebianUpstart = taskKey[File]("creates deb with upstart loader")
lazy val packageDebianSystemd = taskKey[File]("creates deb with systemd loader")
lazy val packageJar = taskKey[File]("creates fat jar")

lazy val packageDebianUpstartTask = packageDebianUpstart := {
  val output = (baseDirectory in root).value / "package" / s"waves-$network-upstart-${version.value}.deb"
  val debianFile = (packageBin in Debian).value
  streams.value.log.info(s"Moving package ${debianFile.getAbsoluteFile} to ${output.getAbsolutePath}")
  IO.move(debianFile, output)
  output
}

lazy val packageDebianSystemdTask = packageDebianSystemd := {
  val output = (baseDirectory in root).value / "package" / s"waves-$network-systemd-${version.value}.deb"
  val debianFile = (packageBin in Debian).value
  streams.value.log.info(s"Moving package ${debianFile.getAbsoluteFile} to ${output.getAbsolutePath}")
  IO.move(debianFile, output)
  output
}

lazy val packageJarTask = packageJar := {
  val output = baseDirectory.value / "package" / s"waves-${version.value}.jar"
  val jarFile = (assembly in Compile).value
  streams.value.log.info(s"Moving package ${jarFile.getAbsoluteFile} to ${output.getAbsolutePath}")
  IO.move(jarFile, output)
  output
}

val commonSettings: Seq[Setting[_]] = Seq(
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

    "-J-Dsun.net.inetaddr.ttl=60"),
  mainClass in assembly := Some("com.wavesplatform.Application"),
  assemblyMergeStrategy in assembly := {
    case "application.conf" => MergeStrategy.concat
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  },
  packageJarTask
)

import NativePackagerHelper._

val debianSettings = Seq(
  maintainer := "wavesplatform.com",
  packageName := {
    if (network == "mainnet") {
      "waves"
    } else if (network == "testnet") {
      "waves-testnet"
    } else {
      throw new IllegalStateException("invalid network")
    }
  },
  packageSummary := "Waves node implementation on top of Scorex",
  packageDescription := "Waves node",
  maintainerScripts in Debian := maintainerScriptsAppend((maintainerScripts in Debian).value)(
    DebianConstants.Postinst -> s"mkdir -p /home/waves && mkdir -p /home/waves/wallet && mkdir -p /home/waves/data && chmod -R 750 /home/waves && chmod -R 750 /var/log/waves/settings.json && chown waves:waves /home/waves"),
  mappings in Universal ++= {
    if (network == "mainnet") {
      Seq((baseDirectory in root).value / "waves-mainnet.json" -> "settings.json")
    } else if (network == "testnet") {
      Seq((baseDirectory in root).value / "waves-testnet.json" -> "settings.json")
    } else {
      throw new IllegalStateException("invalid network")
    }
  },
  mappings in Universal ++= contentOf((baseDirectory in root).value / "src" / "main" / "resources").map(to => (to._1, "conf/" + to._2))
)

val systemdSettings = Seq(
  packageDebianSystemdTask,
  packageBin <<= packageDebianSystemd
)

val upstartSettings = Seq(
  javaOptions in Universal ++= Seq(
    s"-Dlogback.configurationFile=/usr/share/${packageName.value}/conf/logback-to-file.xml",
    s"-Dlogpath=/var/log/${packageName.value}/"
  ),
  packageDebianUpstartTask,
  packageBin <<= packageDebianUpstart
)

lazy val root = project.in(file("."))
  .settings(commonSettings)

lazy val upstart = project.in(file("target/upstart")).dependsOn(root)
  .settings(commonSettings ++ debianSettings ++ upstartSettings)
  .enablePlugins(JavaServerAppPackaging, JDebPackaging, UpstartPlugin)

lazy val systemd = project.in(file("target/systemd")).dependsOn(root)
  .settings(commonSettings ++ debianSettings ++ systemdSettings)
  .enablePlugins(JavaServerAppPackaging, JDebPackaging, SystemdPlugin)

//assembly settings
assemblyJarName in assembly := "waves.jar"
test in assembly := {}
