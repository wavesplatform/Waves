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
  assemblyMergeStrategy in assembly := {
    case "application.conf" => MergeStrategy.concat
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  },
  mainClass in Compile := Some("com.wavesplatform.Application"),
  packageJarTask
)

import com.typesafe.sbt.SbtNativePackager.autoImport.NativePackagerHelper._

val debianSettings = Seq(
  maintainer in Linux := "wavesplatform.com",
  packageName := {
    if (network == "mainnet") {
      "waves"
    } else if (network == "testnet") {
      "waves-testnet"
    } else if (network == "devnet") {
      "waves-devnet"
    } else {
      throw new IllegalStateException("invalid network")
    }
  },
  packageSummary in Linux := "Waves node implementation on top of Scorex",
  packageDescription in Linux := "Waves node",
  maintainerScripts in Debian := maintainerScriptsAppend((maintainerScripts in Debian).value)(
    DebianConstants.Postinst ->
      s"""
         |mkdir -p /home/${packageName.value} &&
         |(mv -n /usr/share/${packageName.value}/settings.conf /etc/${packageName.value}.conf 2>/dev/null || (rm -f /usr/share/${packageName.value}/settings.conf && cp -n /usr/share/${packageName.value}/settings.conf.default /etc/${packageName.value}.conf)) &&
         |ln -sv /etc/${packageName.value}.conf /usr/share/${packageName.value}/settings.conf &&
         |chmod -R 750 /home/${packageName.value} &&
         |chmod -R 750 /usr/share/${packageName.value} &&
         |chmod -R 750 /etc/${packageName.value} &&
         |chmod 750 /etc/${packageName.value}.conf &&
         |chown -R ${packageName.value}:${packageName.value} /etc/${packageName.value}.conf &&
         |chown -R ${packageName.value}:${packageName.value} /usr/share/${packageName.value} &&
         |chown -R ${packageName.value}:${packageName.value} /home/${packageName.value} &&
         |(! test -f /etc/${packageName.value}.json ||
         |java -cp "/usr/share/${packageName.value}/lib/*" com.wavesplatform.settings.LegacyConfigTransformer /etc/${packageName.value}.json > /etc/${packageName.value}.conf &&
         |rm -f /etc/${packageName.value}.json})""".stripMargin),
  debianPackageDependencies in Debian += "java8-runtime-headless",
  mappings in Universal ++= {
    if (network == "mainnet") {
      Seq((baseDirectory in root).value / "waves-mainnet.conf" -> "settings.conf.default")
    } else if (network == "testnet") {
      Seq((baseDirectory in root).value / "waves-testnet.conf" -> "settings.conf.default")
    } else if (network == "devnet") {
      Seq((baseDirectory in root).value / "waves-devnet.conf" -> "settings.conf.default")
    }else {
      throw new IllegalStateException("invalid network")
    }
  },
  bashScriptExtraDefines += s"""addApp "/etc/${packageName.value}.conf"""",
  mappings in Universal ++= contentOf((baseDirectory in root).value / "src" / "main" / "resources").map(to => (to._1, "conf/" + to._2)),
  serviceAutostart in Debian := false,
  executableScriptName := packageName.value
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
